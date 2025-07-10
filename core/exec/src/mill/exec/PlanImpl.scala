package mill.exec

import mill.api.{Plan, Task}
import mill.api.MultiBiMap
import mill.api.TopoSorted
import mill.api.ResolvedTask
import mill.api.UnresolvedTask

import scala.collection.mutable
import scala.reflect.ClassTag

private[mill] object PlanImpl {
  private final case class TaskDetails(
      allInputs: Seq[UnresolvedTask[?]],
      remainingInputs: mutable.HashSet[UnresolvedTask[?]] = new mutable.HashSet,
      var appliedCrossValues: Map[String, String] = Map.empty
  )
  def plan0(goals: Seq[UnresolvedTask[?]]): Plan = {
    val edges = new mutable.HashMap[UnresolvedTask[?], TaskDetails]
    PlanImpl.transitiveNodes(goals.toIndexedSeq) { task =>
      if (!edges.contains(task)) {
        val extraCrossValues = task.task match {
          case c: Task.WithCrossValue[_] =>
            c.crossValues
          case _ =>
            Nil
        }
        val inputs = task.task.inputs.map { inputTask =>
          UnresolvedTask(inputTask, task.crossValues ++ extraCrossValues)
        }
        val details = TaskDetails(inputs)
        details.remainingInputs.addAll(inputs)
        edges(task) = details
      }
      edges(task).allInputs.toSeq
    }

    val appliedCrossValues = new mutable.HashMap[UnresolvedTask[?], ResolvedTask[?]]
    val inputs = new mutable.HashMap[ResolvedTask[?], Seq[ResolvedTask[?]]]
    while (edges.nonEmpty) {
      val (task, details) = edges.find(_._2.remainingInputs.isEmpty).getOrElse {
        sys.error("Cannot happen (no leaf task)")
      }
      edges.remove(task)
      // FIXME Complexity of the whole thing because of that?
      for ((_, details) <- edges)
        details.remainingInputs.remove(task)
      val appliedTask = task.task match {
        case c: Task.CrossValue[?] =>
          val value = task.crossValues.get(c.key).getOrElse {
            sys.error(s"Cross value ${c.key} is undefined")
          }
          ResolvedTask(task.task, task.crossValues + (c.key -> value))
        case _ =>
          ResolvedTask(task.task, task.crossValues)
      }
      appliedCrossValues(task) = appliedTask
      inputs(appliedTask) = details.allInputs.map { inputTask =>
        appliedCrossValues(inputTask)
      }
    }

    val transitive =
      PlanImpl.transitiveNodes(goals.toIndexedSeq.map(appliedCrossValues(_)))(inputs(_))
    val goalSet = goals.map(appliedCrossValues(_)).toSet
    val topoSorted = PlanImpl.topoSorted(transitive, inputs(_))

    val sortedGroups: MultiBiMap[ResolvedTask[?], ResolvedTask[?]] =
      PlanImpl.groupAroundImportantTasks(topoSorted, inputs(_)) {
        // important: all named tasks and those explicitly requested
        case t if t.task.isInstanceOf[Task.Named[Any]] => t
        case t if goalSet.contains(t) => t
      }

    new Plan(
      transitive,
      sortedGroups,
      goals.toIndexedSeq.map(appliedCrossValues(_)),
      inputs.toMap,
      topoSorted
    )
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
  def transitiveNodes[T](sourceNodes: Seq[T])(inputsFor: T => Seq[T]): IndexedSeq[T] = {
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
