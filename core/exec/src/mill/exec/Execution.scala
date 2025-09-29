package mill.exec

import mill.api.daemon.internal.*
import mill.constants.OutFiles.millProfile
import mill.api.*
import mill.internal.{JsonArrayLogger, PrefixLogger}

import java.util.concurrent.{ConcurrentHashMap, ThreadPoolExecutor}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.collection.mutable
import scala.concurrent.*
import scala.reflect.ClassTag

/**
 * Core logic of evaluating tasks, without any user-facing helper methods
 */
private[mill] case class Execution(
    baseLogger: Logger,
    profileLogger: JsonArrayLogger.Profile,
    workspace: os.Path,
    outPath: os.Path,
    externalOutPath: os.Path,
    rootModule: BaseModuleApi,
    classLoaderSigHash: Int,
    classLoaderIdentityHash: Int,
    workerCache: mutable.Map[String, (Int, Val)],
    env: Map[String, String],
    failFast: Boolean,
    ec: Option[ThreadPoolExecutor],
    codeSignatures: Map[String, Int],
    systemExit: ( /* reason */ String, /* exitCode */ Int) => Nothing,
    exclusiveSystemStreams: SystemStreams,
    getEvaluator: () => EvaluatorApi,
    offline: Boolean,
    enableTicker: Boolean
) extends GroupExecution with AutoCloseable {

  // this (shorter) constructor is used from [[MillBuildBootstrap]] via reflection
  def this(
      baseLogger: Logger,
      workspace: java.nio.file.Path,
      outPath: java.nio.file.Path,
      externalOutPath: java.nio.file.Path,
      rootModule: BaseModuleApi,
      classLoaderSigHash: Int,
      classLoaderIdentityHash: Int,
      workerCache: mutable.Map[String, (Int, Val)],
      env: Map[String, String],
      failFast: Boolean,
      ec: Option[ThreadPoolExecutor],
      codeSignatures: Map[String, Int],
      systemExit: ( /* reason */ String, /* exitCode */ Int) => Nothing,
      exclusiveSystemStreams: SystemStreams,
      getEvaluator: () => EvaluatorApi,
      offline: Boolean,
      enableTicker: Boolean
  ) = this(
    baseLogger,
    new JsonArrayLogger.Profile(os.Path(outPath) / millProfile),
    os.Path(workspace),
    os.Path(outPath),
    os.Path(externalOutPath),
    rootModule,
    classLoaderSigHash,
    classLoaderIdentityHash,
    workerCache,
    env,
    failFast,
    ec,
    codeSignatures,
    systemExit,
    exclusiveSystemStreams,
    getEvaluator,
    offline,
    enableTicker
  )

  def withBaseLogger(newBaseLogger: Logger) = this.copy(baseLogger = newBaseLogger)

  /**
   * @param goals The tasks that need to be evaluated
   * @param reporter A function that will accept a module id and provide a listener for build problems in that module
   * @param testReporter Listener for test events like start, finish with success/error
   */
  def executeTasks(
      goals: Seq[UnresolvedTask[?]],
      reporter: Int => Option[CompileProblemReporter] = _ => Option.empty[CompileProblemReporter],
      testReporter: TestReporter = TestReporter.DummyTestReporter,
      logger: Logger = baseLogger,
      serialCommandExec: Boolean = false
  ): mill.api.Result[Execution.Results] = logger.prompt.withPromptUnpaused {
    os.makeDir.all(outPath)

    PathRef.validatedPaths.withValue(new PathRef.ValidatedPaths()) {
      execute0(goals, logger, reporter, testReporter, serialCommandExec)
    }
  }

  private def execute0(
      goals: Seq[UnresolvedTask[?]],
      logger: Logger,
      reporter: Int => Option[
        CompileProblemReporter
      ] /* = _ => Option.empty[CompileProblemReporter]*/,
      testReporter: TestReporter /* = TestReporter.DummyTestReporter*/,
      serialCommandExec: Boolean
  ): mill.api.Result[Execution.Results] = {
    os.makeDir.all(outPath)
    val failed = new AtomicBoolean(false)
    val count = new AtomicInteger(1)
    val rootFailedCount = new AtomicInteger(0) // Track only root failures
    val planningLogger = new PrefixLogger(
      logger0 = baseLogger,
      key0 = Seq("planning"),
      message = "planning"
    )
    val stuff = planningLogger.withPromptLine {
      for (plan <- PlanImpl.planOrErr(goals)) yield {
        val interGroupDeps = Execution.findInterGroupDeps(plan.sortedGroups, plan.inputs)
        val indexToTerminal = plan.sortedGroups.keys().toArray
        ExecutionLogs.logDependencyTree(interGroupDeps, indexToTerminal, outPath, _.displayName)
        // Prepare a lookup tables up front of all the method names that each class owns,
        // and the class hierarchy, so during evaluation it is cheap to look up what class
        // each task belongs to determine of the enclosing class code signature changed.
        val (classToTransitiveClasses, allTransitiveClassMethods) =
          CodeSigUtils.precomputeMethodNamesPerClass(plan.transitive.flatMap(_.asNamed))
        (plan, interGroupDeps, indexToTerminal, classToTransitiveClasses, allTransitiveClassMethods)
      }
    }
    stuff.map {
      case (
            plan,
            interGroupDeps,
            indexToTerminal,
            classToTransitiveClasses,
            allTransitiveClassMethods
          ) =>
        baseLogger.withChromeProfile("execution") {
          val uncached = new ConcurrentHashMap[ResolvedTask[?], Unit]()
          val changedValueHash = new ConcurrentHashMap[ResolvedTask[?], Unit]()
          val prefixes = new ConcurrentHashMap[ResolvedTask[?], Seq[String]]()

          val futures = mutable.Map.empty[ResolvedTask[?], Future[Option[GroupExecution.Results]]]

          def formatHeaderPrefix(countMsg: String, keySuffix: String) =
            s"$countMsg$keySuffix${Execution.formatFailedCount(rootFailedCount.get())}"

          def evaluateTerminals(
              terminals: Seq[ResolvedTask[?]],
              exclusive: Boolean
          ) = {
            val forkExecutionContext =
              ec.fold(ExecutionContexts.RunNow)(new ExecutionContexts.ThreadPool(_))
            implicit val taskExecutionContext =
              if (exclusive) ExecutionContexts.RunNow else forkExecutionContext
            // We walk the task graph in topological order and schedule the futures
            // to run asynchronously. During this walk, we store the scheduled futures
            // in a dictionary. When scheduling each future, we are guaranteed that the
            // necessary upstream futures will have already been scheduled and stored,
            // due to the topological order of traversal.
            for (terminal <- terminals) {
              val deps = interGroupDeps(terminal)

              val group = plan.sortedGroups.lookupKey(terminal)
              val exclusiveDeps = deps.filter(d => d.task.isExclusiveCommand)

              if (!terminal.task.isExclusiveCommand && exclusiveDeps.nonEmpty) {
                val failure = ExecResult.Failure(
                  s"Non-exclusive task ${terminal.task} cannot depend on exclusive task " +
                    exclusiveDeps.map(_.task).mkString(", ")
                )
                val taskResults: Map[ResolvedTask[?], ExecResult.Failing[Nothing]] = group
                  .map(t => (t, failure))
                  .toMap

                futures(terminal) = Future.successful(
                  Some(GroupExecution.Results(taskResults, group.toSeq, false, -1, -1, false, Nil))
                )
              } else {
                futures(terminal) = Future.sequence(deps.map(futures)).map { upstreamValues =>
                  try {
                    val countMsg = mill.api.internal.Util.leftPad(
                      count.getAndIncrement().toString,
                      terminals.length.toString.length,
                      '0'
                    )

                    val keySuffix = s"/${indexToTerminal.size}"

                    val contextLogger = new PrefixLogger(
                      logger0 = logger,
                      key0 = Seq(countMsg),
                      keySuffix = keySuffix,
                      message = terminal.displayName,
                      noPrefix = exclusive
                    )

                    if (enableTicker) prefixes.put(terminal, contextLogger.logKey)
                    contextLogger.withPromptLine {
                      logger.prompt.setPromptHeaderPrefix(formatHeaderPrefix(countMsg, keySuffix))

                      if (failed.get()) None
                      else {
                        val upstreamResults = upstreamValues
                          .iterator
                          .flatMap(_.iterator.flatMap(_.newResults))
                          .toMap

                        val upstreamPathRefs = upstreamValues
                          .iterator
                          .flatMap(_.iterator.flatMap(_.serializedPaths))
                          .toSeq

                        val startTime = System.nanoTime() / 1000

                        val res = executeGroupCached(
                          terminal = terminal,
                          group = plan.sortedGroups.lookupKey(terminal).toSeq,
                          inputsMap = plan.inputs,
                          results = upstreamResults,
                          countMsg = countMsg,
                          zincProblemReporter = reporter,
                          testReporter = testReporter,
                          logger = contextLogger,
                          deps = deps,
                          classToTransitiveClasses,
                          allTransitiveClassMethods,
                          forkExecutionContext,
                          exclusive,
                          upstreamPathRefs
                        )

                        // Count new failures - if there are upstream failures, tasks should be skipped, not failed
                        val newFailures = res.newResults.values.count(r => r.asFailing.isDefined)

                        rootFailedCount.addAndGet(newFailures)

                        // Always show failed count in header if there are failures
                        logger.prompt.setPromptHeaderPrefix(formatHeaderPrefix(countMsg, keySuffix))

                        if (failFast && res.newResults.values.exists(_.asSuccess.isEmpty))
                          failed.set(true)

                        val endTime = System.nanoTime() / 1000
                        val duration = endTime - startTime

                        if (!res.cached) uncached.put(terminal, ())
                        if (res.valueHashChanged) changedValueHash.put(terminal, ())

                        profileLogger.log(
                          terminal.displayName,
                          duration,
                          res.cached,
                          res.valueHashChanged,
                          deps.map(_.displayName),
                          res.inputsHash,
                          res.previousInputsHash
                        )

                        Some(res)
                      }
                    }
                  } catch {
                    // Wrapping the fatal error in a non-fatal exception, so it would be caught by Scala's Future
                    // infrastructure, rather than silently terminating the future and leaving downstream Awaits hanging.
                    case e: Throwable if !scala.util.control.NonFatal(e) =>
                      val nonFatal = new Exception(s"fatal exception occurred: $e", e)
                      // Set the stack trace of the non-fatal exception to the original exception's stack trace
                      // as it actually indicates the location of the error.
                      nonFatal.setStackTrace(e.getStackTrace)
                      throw nonFatal
                  }
                }
              }
            }

            // Make sure we wait for all tasks from this batch to finish before starting the next
            // one, so we don't mix up exclusive and non-exclusive tasks running at the same time
            terminals.map(t => (t, Await.result(futures(t), duration.Duration.Inf)))
          }

          val tasks0 = indexToTerminal.filter(_.task match {
            case _: Task.Command[_] => false
            case _ => true
          })

          val tasksTransitive = PlanImpl.transitiveTasks(Seq.from(tasks0))(plan.inputs(_)).toSet
          val (tasks, leafExclusiveCommands) = indexToTerminal.partition(at =>
            at.task match {
              case t: Task.Named[_] => tasksTransitive.contains(at) || !t.isExclusiveCommand
              case _ => !serialCommandExec
            }
          )

          // Run all non-command tasks according to the threads
          // given but run the commands in linear order
          val nonExclusiveResults = evaluateTerminals(tasks, exclusive = false)

          val exclusiveResults = evaluateTerminals(leafExclusiveCommands, exclusive = true)

          logger.prompt.clearPromptStatuses()

          val finishedOptsMap = (nonExclusiveResults ++ exclusiveResults).toMap

          ExecutionLogs.logInvalidationTree(
            interGroupDeps,
            indexToTerminal,
            outPath,
            uncached,
            changedValueHash,
            _.displayName,
            drop = task =>
              task.task match {
                case _: Task.Input[?] => true
                case _ => false
              }
          )

          val results0: Array[(ResolvedTask[?], ExecResult[(Val, Int)])] = indexToTerminal
            .map { t =>
              finishedOptsMap(t) match {
                case None => (t, ExecResult.Skipped)
                case Some(res) =>
                  Tuple2(
                    t,
                    (Seq(t) ++ plan.sortedGroups.lookupKey(t))
                      .flatMap { t0 => res.newResults.get(t0) }
                      .sortBy(!_.isInstanceOf[ExecResult.Failing[?]])
                      .head
                  )

              }
            }

          val results: Map[ResolvedTask[?], ExecResult[(Val, Int)]] = results0.toMap

          import scala.collection.JavaConverters._
          Execution.Results(
            plan.goals,
            plan.goals.toIndexedSeq.map(results(_).map(_._1)),
            finishedOptsMap.values.flatMap(_.toSeq.flatMap(_.newEvaluated)).toSeq,
            results.map { case (k, v) => (k, v.map(_._1)) },
            prefixes.asScala.toMap
          )
        }
    }
  }

  def close(): Unit = {
    profileLogger.close()
  }
}

private[mill] object Execution {

  /**
   * Format a failed count as a string to be used in status messages.
   * Returns ", N failed" if count > 0, otherwise an empty string.
   */
  def formatFailedCount(count: Int): String = {
    if (count > 0) s", $count failed" else ""
  }

  def findInterGroupDeps[T: ClassTag](sortedGroups: MultiBiMap[T, T], inputs: Map[T, Seq[T]])
      : Map[T, Seq[T]] = {
    val out = Map.newBuilder[T, Seq[T]]
    for ((terminal, group) <- sortedGroups) {
      val groupSet = group.toSet
      out.addOne(
        terminal -> groupSet
          .flatMap(
            inputs(_).collect { case f if !groupSet.contains(f) => sortedGroups.lookupValue(f) }
          )
          .toArray
      )
    }
    out.result()
  }
  private[Execution] case class Results(
      goals: Seq[ResolvedTask[?]],
      results: Seq[ExecResult[Val]],
      uncached: Seq[ResolvedTask[?]],
      transitiveResults: Map[ResolvedTask[?], ExecResult[Val]],
      override val transitivePrefixes: Map[ResolvedTask[?], Seq[String]]
  ) extends mill.api.ExecutionResults
}
