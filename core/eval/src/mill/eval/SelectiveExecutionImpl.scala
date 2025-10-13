package mill.eval

import mill.api.daemon.internal.TestReporter
import mill.api.{ExecResult, Result, Val}
import mill.constants.OutFiles
import mill.api.SelectiveExecution.ChangedTasks
import mill.api.*
import mill.exec.{CodeSigUtils, Execution, PlanImpl}
import mill.internal.SpanningForest
import mill.internal.SpanningForest.breadthFirst

private[mill] class SelectiveExecutionImpl(evaluator: Evaluator)
    extends mill.api.SelectiveExecution {

  def computeHashCodeSignatures(
      transitiveNamed: Seq[ResolvedNamedTask[?]],
      codeSignatures: Map[String, Int]
  ): Map[String, Int] = {

    val (classToTransitiveClasses, allTransitiveClassMethods) =
      CodeSigUtils.precomputeMethodNamesPerClass(transitiveNamed)

    lazy val constructorHashSignatures = CodeSigUtils
      .constructorHashSignatures(codeSignatures)

    transitiveNamed
      .map { namedTask =>
        namedTask.displayName -> CodeSigUtils
          .codeSigForTask(
            namedTask.task,
            classToTransitiveClasses,
            allTransitiveClassMethods,
            codeSignatures,
            constructorHashSignatures
          )
          .sum
      }
      .toMap
  }

  def computeDownstream(
      plan: Plan,
      transitiveNamed: Seq[ResolvedNamedTask[?]],
      oldHashes: SelectiveExecution.Metadata,
      newHashes: SelectiveExecution.Metadata
  ): (Set[ResolvedTask[?]], Seq[ResolvedTask[Any]]) = {
    val namesToTasks = transitiveNamed.map(t => (t.displayName -> t)).toMap

    def diffMap[K, V](lhs: Map[K, V], rhs: Map[K, V]) = {
      (lhs.keys ++ rhs.keys)
        .iterator
        .distinct
        .filter { k => lhs.get(k) != rhs.get(k) }
        .toSet
    }

    val changedInputNames = diffMap(oldHashes.inputHashes, newHashes.inputHashes)
    val changedCodeNames = diffMap(
      computeHashCodeSignatures(transitiveNamed, oldHashes.codeSignatures),
      computeHashCodeSignatures(transitiveNamed, newHashes.codeSignatures)
    )

    val changedRootTasks = (changedInputNames ++ changedCodeNames)
      .flatMap(namesToTasks.get(_))
      .map(_.asTask)

    val allNodes = breadthFirst(transitiveNamed.map(_.asTask))(plan.inputs(_))
    val downstreamEdgeMap = SpanningForest.reverseEdges(allNodes.map(t => (t, plan.inputs(t))))

    (
      changedRootTasks,
      breadthFirst(changedRootTasks) { t =>
        downstreamEdgeMap.getOrElse(t, Nil)
      }
    )
  }

  def saveMetadata(metadata: SelectiveExecution.Metadata): Unit = {
    os.write.over(
      evaluator.outPath / OutFiles.millSelectiveExecution,
      upickle.write(metadata, indent = 2)
    )
  }

  def computeChangedTasks(
      tasks: Seq[String]
  ): Result[ChangedTasks] = {
    for {
      tasks <- evaluator.resolveTasks(
        tasks,
        SelectMode.Separated,
        evaluator.allowPositionalCommandArgs
      )
      computedMetadata <- SelectiveExecutionImpl.Metadata.compute(evaluator, tasks.map(_.asTask))
      res <- computeChangedTasks0(tasks.map(_.asTask), computedMetadata)
        // If we did not have the metadata, presume everything was changed.
        .getOrElse(Result.Success(ChangedTasks.all(tasks.map(_.resolved()))))
    } yield res
  }

  /**
   * @return [[None]] when the metadata file is empty.
   * @note throws if the metadata file does not exist.
   */
  def computeChangedTasks0(
      tasks: Seq[UnresolvedTask[?]],
      computedMetadata: SelectiveExecution.Metadata.Computed
  ): Option[Result[ChangedTasks]] = {
    val oldMetadataTxt = os.read(evaluator.outPath / OutFiles.millSelectiveExecution)

    // We allow to clear the selective execution metadata to rerun all tasks.
    //
    // You would think that removing the file achieves the same result, however, blanking the file indicates that
    // this was intentional and you did not simply forgot to run `selective.prepare` beforehand.
    if (oldMetadataTxt == "") None
    else Some {
      PlanImpl.planOrErr(tasks).map { plan =>
        val transitiveNamed = plan.transitive.flatMap(_.asNamed)
        val oldMetadata = upickle.read[SelectiveExecution.Metadata](oldMetadataTxt)
        val (changedRootTasks, downstreamTasks) =
          evaluator.selective.computeDownstream(
            plan,
            transitiveNamed,
            oldMetadata,
            computedMetadata.metadata
          )

        ChangedTasks(
          plan.goals.flatMap(_.asNamed),
          changedRootTasks.flatMap(_.asNamed),
          downstreamTasks.flatMap(_.asNamed)
        )
      }
    }
  }

  def resolve0(tasks: Seq[String]): Result[Array[String]] = {
    for {
      resolved <- evaluator.resolveTasks(tasks, SelectMode.Separated)
      changedTasks <- this.computeChangedTasks(tasks)
    } yield {
      val resolvedSet = resolved.map(_.render).toSet
      val downstreamSet = changedTasks.downstreamTasks.map(_.render).toSet
      resolvedSet.intersect(downstreamSet).toArray.sorted
    }
  }

  def resolveChanged(tasks: Seq[String]): Result[Seq[String]] = {
    for (changedTasks <- this.computeChangedTasks(tasks)) yield {
      changedTasks.changedRootTasks.map(_.task.ctx.segments.render).toSeq.sorted
    }
  }

  def resolveTree(tasks: Seq[String]): Result[ujson.Value] = {
    for {
      changedTasks <- this.computeChangedTasks(tasks)
      taskSet = changedTasks.downstreamTasks.map(_.asTask).toSet[ResolvedTask[?]]
      plan <- PlanImpl.planOrErr(changedTasks.downstreamTasks.map(_.asTask.asUnappliedTask))
    } yield {
      val indexToTerminal = plan
        .sortedGroups
        .keys()
        .toArray
        .filter(t => taskSet.contains(t))
        .sortBy(_.toString) // Sort to ensure determinism

      val interGroupDeps = Execution.findInterGroupDeps(plan.sortedGroups, plan.inputs)
      val reverseInterGroupDeps = SpanningForest.reverseEdges(
        interGroupDeps.toSeq.sortBy(_._1.toString) // sort to ensure determinism
      )

      val ( /*vertexToIndex*/ _, edgeIndices) =
        SpanningForest.graphMapToIndices(indexToTerminal, reverseInterGroupDeps)

      val json = SpanningForest.writeJson(
        indexEdges = edgeIndices,
        interestingIndices = indexToTerminal.indices.toSet,
        render = indexToTerminal(_).task.toString
      )

      // Simplify the tree structure to only show the direct paths to the tasks
      // resolved directly, removing the other branches, since those tasks are
      // the ones that the user probably cares about
      val resolvedTaskLabels = changedTasks.resolved.map(_.task.ctx.segments.render).toSet
      def simplifyJson(j: ujson.Obj): Option[ujson.Obj] = {
        val map = j.value.flatMap {
          case (k, v: ujson.Obj) =>
            simplifyJson(v)
              .map((k, _))
              .orElse(Option.when(resolvedTaskLabels.contains(k)) { k -> v })
          case _ => ???
        }
        Option.when(map.nonEmpty)(ujson.Obj.from(map))
      }

      simplifyJson(json).getOrElse(ujson.Obj())
    }
  }

  def computeMetadata(
      tasks: Seq[ResolvedNamedTask[?]]
  ): mill.api.Result[SelectiveExecution.Metadata.Computed] =
    SelectiveExecutionImpl.Metadata.compute(evaluator, tasks.map(_.asTask.asUnappliedTask))
}
object SelectiveExecutionImpl {
  object Metadata {
    def compute(
        evaluator: Evaluator,
        tasks: Seq[UnresolvedTask[?]]
    ): mill.api.Result[SelectiveExecution.Metadata.Computed] = {
      PlanImpl.planOrErr(tasks).map { plan =>
        compute0(evaluator, plan.transitive.flatMap(_.asNamed))
      }
    }

    def compute0(
        evaluator: Evaluator,
        transitiveNamed: Seq[ResolvedNamedTask[?]]
    ): SelectiveExecution.Metadata.Computed = {
      val results: Map[ResolvedNamedTask[?], mill.api.Result[Val]] = transitiveNamed
        .map(t => (t, t.task))
        .collect { case (t, task: Task.Input[_]) =>
          val args = task.inputs.toVector.map { crossValueTask =>
            val str = t.crossValues.getOrElse(
              crossValueTask.key,
              s"Missing cross value ${crossValueTask.key} when evaluating $t (available cross-values: ${t.crossValues.toSeq.sorted})"
            )
            crossValueTask.valuesMap(str)
          }
          val ctx = new mill.api.TaskCtx.Impl(
            args = args,
            dest0 = () => null,
            log = evaluator.baseLogger,
            env = evaluator.env,
            reporter = _ => None,
            testReporter = TestReporter.DummyTestReporter,
            workspace = evaluator.workspace,
            outFolder = evaluator.outPath,
            _systemExitWithReason = (reason, exitCode) =>
              throw Exception(s"systemExit called: reason=$reason, exitCode=$exitCode"),
            fork = null,
            jobs = evaluator.effectiveThreadCount,
            offline = evaluator.offline,
            crossValues = t.crossValues
          )
          t -> task.evaluate(ctx).map(Val(_))
        }
        .toMap

      val inputHashes = results.map {
        case (task, execResultVal) => (task.displayName, execResultVal.get.value.hashCode)
      }
      SelectiveExecution.Metadata.Computed(
        new SelectiveExecution.Metadata(
          inputHashes,
          evaluator.codeSignatures
        ),
        results.map { case (k, v) => (k.asTask, ExecResult.Success(v.get)) }
      )
    }
  }

}
