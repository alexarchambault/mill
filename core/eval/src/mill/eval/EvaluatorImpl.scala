package mill.eval

import mill.api.daemon.internal.{CompileProblemReporter, ExecutionResultsApi, TestReporter}
import mill.constants.OutFiles
import mill.constants.OutFiles.*
import mill.api.{PathRef, *}
import mill.api.internal.{ResolveChecker, Resolved, RootModule0}
import mill.api.daemon.Watchable
import mill.exec.{Execution, PlanImpl}
import mill.internal.PrefixLogger
import mill.resolve.Resolve
import mill.api.UnresolvedTask
import mill.api.ResolvedTask
import scala.reflect.ClassTag

/**
 * [[EvaluatorImpl]] is the primary API through which a user interacts with the Mill
 * evaluation process. The various phases of evaluation as methods they can call:
 *
 * 1. [[resolveSegments]]/[[resolveTasks]]
 * 2. [[plan]]
 * 3. [[execute]]/[[execute]],
 *
 * As well as [[evaluate]] which does all of these phases one after another
 */

final class EvaluatorImpl private[mill] (
    private[mill] val allowPositionalCommandArgs: Boolean,
    private[mill] val selectiveExecution: Boolean = false,
    private val execution: Execution
) extends Evaluator {

  private[mill] def workspace = execution.workspace
  private[mill] def baseLogger = execution.baseLogger
  private[mill] def outPath = execution.outPath
  private[mill] def codeSignatures = execution.codeSignatures
  private[mill] def rootModule =
    execution.rootModule match {
      case m: RootModule0 => m
      case _ => sys.error("should not happen")
    }
  private[mill] def workerCache = execution.workerCache
  private[mill] def env = execution.env
  private[mill] def effectiveThreadCount = execution.effectiveThreadCount
  override private[mill] def offline: Boolean = execution.offline

  def withBaseLogger(newBaseLogger: Logger): Evaluator = new EvaluatorImpl(
    allowPositionalCommandArgs,
    selectiveExecution,
    execution.withBaseLogger(newBaseLogger)
  )

  /**
   * Takes query selector tokens and resolves them to a list of [[Segments]]
   * representing concrete tasks or modules that match that selector
   */
  def resolveSegments(
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      allowPositionalCommandArgs: Boolean = false,
      resolveToModuleTasks: Boolean = false
  ): mill.api.Result[List[Segments]] = {
    os.checker.withValue(ResolveChecker(workspace)) {
      Resolve.Segments.resolve(
        rootModule,
        scriptArgs,
        selectMode,
        allowPositionalCommandArgs,
        resolveToModuleTasks
      )
    }
  }
  override def resolveRaw(
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      allowPositionalCommandArgs: Boolean = false,
      resolveToModuleTasks: Boolean = false
  ): mill.api.Result[List[Resolved]] = {
    os.checker.withValue(ResolveChecker(workspace)) {
      Resolve.Raw.resolve(
        rootModule,
        scriptArgs,
        selectMode,
        allowPositionalCommandArgs,
        resolveToModuleTasks
      )
    }
  }

  /**
   * Takes query selector tokens and resolves them to a list of [[Task.Named]]s
   * representing concrete tasks or modules that match that selector
   */
  def resolveTasks(
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      allowPositionalCommandArgs: Boolean = false,
      resolveToModuleTasks: Boolean = false
  ): mill.api.Result[List[Task.Named[?]]] = {
    os.checker.withValue(ResolveChecker(workspace)) {
      Evaluator.withCurrentEvaluator(this) {
        Resolve.Tasks.resolve(
          rootModule,
          scriptArgs,
          selectMode,
          allowPositionalCommandArgs,
          resolveToModuleTasks
        )
      }
    }
  }
  def resolveModulesOrTasks(
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      allowPositionalCommandArgs: Boolean = false,
      resolveToModuleTasks: Boolean = false
  ): mill.api.Result[List[Either[Module, Task.Named[?]]]] = {
    os.checker.withValue(ResolveChecker(workspace)) {
      Evaluator.withCurrentEvaluator(this) {
        Resolve.Inspect.resolve(
          rootModule,
          scriptArgs,
          selectMode,
          allowPositionalCommandArgs,
          resolveToModuleTasks
        )
      }
    }
  }

  /**
   * Takes a sequence of [[Task]]s and returns a [[PlanImpl]] containing the
   * transitive upstream tasks necessary to evaluate those provided.
   */
  def plan(tasks: Seq[UnresolvedTask[?]]): Plan = PlanImpl.plan0(tasks)

  def transitiveTasks0(sourceNodes: Seq[ResolvedTask[_]])(
      inputsFor: ResolvedTask[_] => Seq[ResolvedTask[_]]
  ): IndexedSeq[ResolvedTask[_]] =
    PlanImpl.transitiveNodes(sourceNodes)(inputsFor)

  def topoSorted(transitiveTasks: IndexedSeq[Task[?]]): TopoSorted[Task[?]] = {
    PlanImpl.topoSorted(transitiveTasks, _.inputs)
  }
  def topoSorted0[T: ClassTag](transitiveTasks: IndexedSeq[T], inputs: T => Seq[T]): TopoSorted[T] =
    PlanImpl.topoSorted(transitiveTasks, inputs)

  def groupAroundImportantTasks[T](topoSortedTasks: TopoSorted[Task[?]])(important: PartialFunction[
    Task[?],
    T
  ]) = {
    PlanImpl.groupAroundImportantTasks(topoSortedTasks, _.inputs)(important)
  }

  def groupAroundImportantTasks0[T](
      topoSortedTasks: TopoSorted[ResolvedTask[?]],
      plan: Plan
  )(important: PartialFunction[
    ResolvedTask[?],
    T
  ]) = {
    PlanImpl.groupAroundImportantTasks(topoSortedTasks, plan.inputs(_))(important)
  }

  def execute[T](
      tasks: Seq[UnresolvedTask[T]],
      reporter: Int => Option[CompileProblemReporter] = _ => Option.empty[CompileProblemReporter],
      testReporter: TestReporter = TestReporter.DummyTestReporter,
      logger: Logger = baseLogger,
      serialCommandExec: Boolean = false,
      selectiveExecution: Boolean = false
  ): Evaluator.Result[T] = {

    val selectiveExecutionEnabled = selectiveExecution && !tasks.exists(_.task.isExclusiveCommand)

    val selectedTasksOrErr =
      if (!selectiveExecutionEnabled) (tasks, Map.empty, None)
      else {
        val (named, unnamed) =
          tasks.map(t => (t, t.task)).partitionMap {
            case (t, _: Task.Named[?]) => Left(t); case (t, _) => Right(t)
          }
        val newComputedMetadata =
          SelectiveExecutionImpl.Metadata.compute(this, named)

        val selectiveExecutionStoredData = for {
          _ <- Option.when(os.exists(outPath / OutFiles.millSelectiveExecution))(())
          changedTasks <- this.selective.computeChangedTasks0(
            named,
            newComputedMetadata
          )
        } yield changedTasks

        selectiveExecutionStoredData match {
          case None =>
            // Ran when previous selective execution metadata is not available, which happens the first time you run
            // selective execution.
            (tasks, Map.empty, Some(newComputedMetadata.metadata))
          case Some(changedTasks) =>
            val selectedSet = changedTasks.downstreamTasks.map(_.displayName).toSet

            (
              unnamed ++ named.filter(t =>
                t.task.isExclusiveCommand ||
                  // FIXME We need to compute ResolvedTask-s out of named using Plan here
                  selectedSet(ResolvedTask(t.task, t.crossValues).displayName)
              ),
              newComputedMetadata.results,
              Some(newComputedMetadata.metadata)
            )
        }
      }

    selectedTasksOrErr match {
      case (selectedTasks, selectiveResults, maybeNewMetadata) =>
        val evaluated: ExecutionResults =
          execution.executeTasks(
            selectedTasks,
            reporter,
            testReporter,
            logger,
            serialCommandExec
          )
        @scala.annotation.nowarn("msg=cannot be checked at runtime")
        val watched = (evaluated.transitiveResults.iterator ++ selectiveResults)
          .toSeq
          .map {
            case (t, res) =>
              (t, t.task, res)
          }
          .collect {
            case (_, _: Task.Sources, ExecResult.Success(Val(ps: Seq[PathRef]))) =>
              ps.map(r => Watchable.Path(r.path.toNIO, r.quick, r.sig))
            case (_, _: Task.Source, ExecResult.Success(Val(p: PathRef))) =>
              Seq(Watchable.Path(p.path.toNIO, p.quick, p.sig))
            case (task, t: Task.Input[_], result) =>

              val ctx = new mill.api.TaskCtx.Impl(
                args = Vector(),
                dest0 = () => null,
                log = logger,
                env = this.execution.env,
                reporter = reporter,
                testReporter = testReporter,
                workspace = workspace,
                _systemExitWithReason = (reason, exitCode) =>
                  throw Exception(s"systemExit called: reason=$reason, exitCode=$exitCode"),
                fork = null,
                jobs = execution.effectiveThreadCount,
                offline = offline,
                crossValues = task.crossValues
              )
              val pretty = t.ctx0.fileName + ":" + t.ctx0.lineNum
              Seq(Watchable.Value(
                () => t.evaluate(ctx).hashCode(),
                result.map(_.value).hashCode(),
                pretty
              ))
          }
          .flatten

        maybeNewMetadata.foreach { newMetadata =>
          val allInputHashes = newMetadata.inputHashes
          this.selective.saveMetadata(
            SelectiveExecution.Metadata(allInputHashes, codeSignatures)
          )
        }

        val errorStr = ExecutionResultsApi.formatFailing(evaluated)
        evaluated.transitiveFailing.size match {
          case 0 =>
            Evaluator.Result(
              watched,
              mill.api.Result.Success(evaluated.values.map(_._1.asInstanceOf[T])),
              evaluated.goals,
              evaluated
            )
          case n =>
            Evaluator.Result(
              watched,
              mill.api.Result.Failure(s"$n tasks failed\n$errorStr"),
              evaluated.goals,
              evaluated
            )
        }
    }
  }

  /**
   * Evaluates the given query selector, performing [[resolveTasks]] and [[execute]]
   * internally, and returning the [[Evaluator.Result]] containing the output
   */
  def evaluate(
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      reporter: Int => Option[CompileProblemReporter] = _ => None,
      selectiveExecution: Boolean = false
  ): mill.api.Result[Evaluator.Result[Any]] = {
    val promptLineLogger = new PrefixLogger(
      logger0 = baseLogger,
      key0 = Seq("resolve"),
      message = "resolve " + scriptArgs.mkString(" ")
    )

    val resolved = promptLineLogger.withPromptLine {
      os.checker.withValue(ResolveChecker(workspace)) {
        Evaluator.withCurrentEvaluator(this) {
          Resolve.Tasks.resolve(
            rootModule,
            scriptArgs,
            selectMode,
            allowPositionalCommandArgs
          )
        }
      }
    }

    // FIXME Get via scriptArgs?
    val crossValues = Map.empty[String, String]

    for (tasks <- resolved)
      yield execute(
        Seq.from(tasks.map(_.unresolved(crossValues))),
        reporter = reporter,
        selectiveExecution = selectiveExecution
      )
  }

  def close(): Unit = execution.close()

  val selective = new mill.eval.SelectiveExecutionImpl(this)
}
