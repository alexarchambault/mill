package mill.exec

import mill.api.{Plan, ResolvedTask, Task, UnresolvedTask}
import mill.api.MultiBiMap
import mill.api.TopoSorted

import scala.collection.mutable
import scala.reflect.ClassTag
import mill.api.daemon.Result

private[mill] object PlanImpl {
  private final case class TaskDetails(
      allInputs: Seq[UnresolvedTask[?]],
      remainingInputs: mutable.HashSet[UnresolvedTask[?]] = new mutable.HashSet,
      var appliedCrossValues: Map[String, String] = Map.empty
  )

  private def reconcileInputs(inputs: Seq[Seq[ResolvedTask[?]]])
      : Seq[(Seq[ResolvedTask[?]], Map[String, String])] = {

    def helper(
        inputs0: List[Seq[ResolvedTask[?]]],
        crossValues: Map[String, String]
    ): Seq[(List[ResolvedTask[?]], Map[String, String])] =
      inputs0 match {
        case Nil => Seq((Nil, crossValues))
        case h :: t =>
          val filtered = h.filter(_.crossValues.forall { case (k, v) =>
            crossValues.get(k).forall(_ == v)
          })
          if (filtered.isEmpty) Nil // FIXME Report error to users
          else
            filtered.flatMap { t0 =>
              // no keys with different values here thanks to how filtered is computed
              val updatedCrossValues = crossValues ++ t0.crossValues
              helper(t, updatedCrossValues).map {
                case (l, m) =>
                  (t0 :: l, m)
              }
            }
      }

    helper(inputs.toList, Map.empty)
  }

  def planOrErr(goals: Seq[UnresolvedTask[?]]): Result[Plan] = {
    val edges = new mutable.HashMap[UnresolvedTask[?], TaskDetails]
    PlanImpl.transitiveTasks(goals.toIndexedSeq) { task =>
      if (!edges.contains(task)) {
        val extraCrossValues = task.task match {
          case c: Task.WithCrossValue[_] =>
            c.crossValues
          case _ =>
            Nil
        }
        val inputs = task.task.inputs.map { inputTask =>
          inputTask.unresolved(task.crossValues ++ extraCrossValues)
        }
        val details = TaskDetails(inputs)
        details.remainingInputs.addAll(inputs)
        edges(task) = details
      }
      edges(task).allInputs.toSeq
    }

    val appliedCrossValues =
      new mutable.HashMap[UnresolvedTask[?], Either[Seq[String], Seq[ResolvedTask[?]]]]
    val inputs = new mutable.HashMap[ResolvedTask[?], Seq[ResolvedTask[?]]]
    while (edges.nonEmpty) {
      val (task, details) = edges.find(_._2.remainingInputs.isEmpty).getOrElse {
        sys.error("Cannot happen (no leaf task)")
      }
      edges.remove(task)
      // FIXME Complexity of the whole thing because of that?
      for ((_, details) <- edges)
        details.remainingInputs.remove(task)
      val maybeAppliedTask: Either[Seq[String], Seq[ResolvedTask[Any]]] = task.task match {
        case c: Task.CrossValue =>
          task.crossValues.get(c.key) match {
            case None =>
              Right {
                c.allowedValues.map { value =>
                  val appliedTask = c.resolved(Map(c.key -> value))
                  inputs(appliedTask) = Nil
                  appliedTask
                }
              }
            case Some(value) =>
              if (c.allowedValues.contains(value)) {
                val appliedTask = task.task.resolved(Map(c.key -> value))
                inputs(appliedTask) = Nil
                Right(Seq(appliedTask))
              } else
                Left(Seq(
                  s"$c doesn't accept value '$value' (allowed values: ${c.allowedValues.mkString(", ")})"
                ))
          }
        case _ =>
          val resolvedInputsOrErrors = details.allInputs.map(appliedCrossValues(_))
          val resolvedInputs = resolvedInputsOrErrors.collect {
            case Right(resolveInputTask) =>
              resolveInputTask
          }
          val allLefts = resolvedInputsOrErrors
            .collect {
              case Left(err) => err
            }
            .flatten
            .distinct
          if (resolvedInputs.isEmpty && allLefts.nonEmpty)
            Left(allLefts)
          else {
            val reconcileInputs0 = reconcileInputs(resolvedInputs)
            Right {
              reconcileInputs0.map {
                case (resolvedInputs0, retainedCrossValues) =>
                  val appliedTask = task.task.resolved(retainedCrossValues)
                  inputs(appliedTask) = resolvedInputs0
                  appliedTask
              }
            }
          }
      }
      appliedCrossValues(task) = maybeAppliedTask
    }

    val resolvedGoals = goals.toIndexedSeq.flatMap(appliedCrossValues(_).toSeq.flatten)
    val ignored = goals.flatMap(appliedCrossValues(_).left.toSeq).flatten.distinct
    val transitive = PlanImpl.transitiveTasks(resolvedGoals)(inputs(_))
    val resolvedGoalSet = resolvedGoals.toSet
    val topoSorted = PlanImpl.topoSorted(transitive, inputs(_))

    val sortedGroups: MultiBiMap[ResolvedTask[?], ResolvedTask[?]] =
      PlanImpl.groupAroundImportantTasks(topoSorted, inputs(_)) {
        // important: all named tasks and those explicitly requested
        case t if t.task.isInstanceOf[Task.Named[Any]] => t
        case t if resolvedGoalSet.contains(t) => t
      }

    val plan = new Plan(
      transitive,
      sortedGroups,
      resolvedGoals,
      inputs.toMap,
      topoSorted,
      ignored
    )
    if (plan.goals.isEmpty && plan.ignoredDetails.nonEmpty)
      Result.Failure(plan.ignoredDetails.mkString(", "))
    else
      Result.Success(plan)
  }

  /**
   * The `values` [[Agg]] is guaranteed to be topological sorted and cycle free.
   * That's why the constructor is package private.
   *
   * @see [[PlanImpl.topoSorted]]
   */

  def groupAroundImportantTasks[T, TaskT: ClassTag](
      topoSortedTasks: TopoSorted[TaskT],
      inputs: TaskT => Seq[TaskT]
  )(important: PartialFunction[TaskT, T]): MultiBiMap[T, TaskT] = {

    val output = new MultiBiMap.Mutable[T, TaskT]()
    val topoSortedIndices = topoSortedTasks.values.zipWithIndex.toMap
    for (task <- topoSortedTasks.values) {
      for (t <- important.lift(task)) {

        val transitiveTasks = collection.mutable.Map[TaskT, Int]()

        def rec(t: TaskT): Unit = {
          if (transitiveTasks.contains(t)) () // do nothing
          else if (important.isDefinedAt(t) && t != task) () // do nothing
          else {
            transitiveTasks.put(t, topoSortedIndices(t))
            inputs(t).foreach(rec)
          }
        }

        rec(task)
        val out = transitiveTasks.toArray
        out.sortInPlaceBy(_._2)
        output.addAll(t, out.map(_._1))
      }
    }

    output
  }

  /**
   * Collects all transitive dependencies (nodes) of the given nodes,
   * including the given nodes.
   */
  def transitiveTasks[T](sourceNodes: Seq[T])(inputsFor: T => Seq[T]): IndexedSeq[T] = {
    val transitiveNodes = collection.mutable.LinkedHashSet[T]()
    def rec(t: T): Unit = {
      if (transitiveNodes.contains(t)) {} // do nothing
      else {
        transitiveNodes.add(t)
        inputsFor(t).foreach(rec)
      }
    }

    sourceNodes.foreach(rec)
    transitiveNodes.toIndexedSeq
  }

  /**
   * Takes the given tasks, finds all the targets they transitively depend
   * on, and sort them topologically. Fails if there are dependency cycles
   */
  def topoSorted[T: ClassTag](
      transitiveTasks: IndexedSeq[T],
      inputs: T => Seq[T]
  ): TopoSorted[T] = {

    val indexed = transitiveTasks
    val taskIndices = indexed.zipWithIndex.toMap

    val numberedEdges = transitiveTasks.map(inputs(_).collect(taskIndices).toArray)

    val sortedClusters = mill.internal.Tarjans(numberedEdges)
    assert(sortedClusters.count(_.length > 1) == 0, sortedClusters.filter(_.length > 1))
    new TopoSorted(sortedClusters.map(_(0)).map(indexed))
  }
}
