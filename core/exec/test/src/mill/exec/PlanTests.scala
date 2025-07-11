package mill.exec

import mill.api.Task
import mill.api.Task.Simple
import mill.api.TestGraphs
import utest.*

import scala.collection.mutable
import mill.api.UnresolvedTask
import mill.api.ResolvedTask

object PlanTests extends TestSuite {
  def checkTopological(
      tasks: Seq[ResolvedTask[?]],
      inputs: Map[ResolvedTask[?], Seq[ResolvedTask[?]]]
  ) = {
    val seen = mutable.Set.empty[ResolvedTask[?]]
    for (t <- tasks.reverseIterator) {
      seen.add(t)
      for (upstream <- inputs(t)) {
        assert(!seen(upstream))
      }
    }
  }

  val tests = Tests {

    import TestGraphs._

    test("topoSortedTransitiveTasks") {
      def check(tasks: Seq[UnresolvedTask[?]], expected: Seq[ResolvedTask[?]]) = {
        val result = PlanImpl.plan0(tasks)
        checkTopological(result.goals, result.inputs)
        assert(result == expected)
      }

      test("singleton") - check(
        tasks = Seq(singleton.single).map(UnresolvedTask(_, Map.empty)),
        expected = Seq(singleton.single).map(ResolvedTask(_, Map.empty))
      )
      test("backtickIdentifiers") - check(
        tasks = Seq(bactickIdentifiers.`a-down-task`).map(UnresolvedTask(_, Map.empty)),
        expected = Seq(
          bactickIdentifiers.`up-task`,
          bactickIdentifiers.`a-down-task`
        ).map(ResolvedTask(_, Map.empty))
      )
      test("pair") - check(
        tasks = Seq(pair.down).map(UnresolvedTask(_, Map.empty)),
        expected = Seq(pair.up, pair.down).map(ResolvedTask(_, Map.empty))
      )
      test("anonTriple") - check(
        tasks = Seq(anonTriple.down).map(UnresolvedTask(_, Map.empty)),
        expected = Seq(
          anonTriple.up,
          anonTriple.down.inputs(0),
          anonTriple.down
        ).map(ResolvedTask(_, Map.empty))
      )
      test("diamond") - check(
        tasks = Seq(diamond.down).map(UnresolvedTask(_, Map.empty)),
        expected =
          Seq(diamond.up, diamond.left, diamond.right, diamond.down).map(ResolvedTask(_, Map.empty))
      )
      test("anonDiamond") - check(
        tasks = Seq(diamond.down).map(UnresolvedTask(_, Map.empty)),
        expected = Seq(
          diamond.up,
          diamond.down.inputs(0),
          diamond.down.inputs(1),
          diamond.down
        ).map(ResolvedTask(_, Map.empty))
      )
    }
    test("groupAroundNamedTasks") {
      def check[T, R <: Simple[Int]](base: T)(
          task: T => R,
          important0: Seq[T => Simple[?]],
          expected: Seq[(R, Int)]
      ) = {

        val plan = PlanImpl.plan0(Seq(UnresolvedTask(task(base), Map.empty)))

        val important = important0.map(_(base))
        val grouped = PlanImpl.groupAroundImportantTasks(plan.topoSorted, plan.inputs(_)) {
          case ResolvedTask(t: Task.Computed[_], _) if important.contains(t) => t: Simple[?]
        }
        val flattened = Seq.from(grouped.values().flatten)

        checkTopological(flattened, plan.inputs)
        for ((terminal, expectedSize) <- expected) {
          val grouping = grouped.lookupKey(terminal)
          assert(
            grouping.size == expectedSize,
            grouping
              .filter(_._2.isEmpty)
              .flatMap(_.task.asSimple: Option[Simple[?]])
              .filter(important.contains) ==
              Seq(terminal)
          )
        }
      }

      test("singleton") - check(singleton)(
        _.single,
        Seq(_.single),
        Seq(singleton.single -> 1)
      )
      test("backtickIdentifiers") - check(bactickIdentifiers)(
        _.`a-down-task`,
        Seq(_.`up-task`, _.`a-down-task`),
        Seq(
          bactickIdentifiers.`up-task` -> 1,
          bactickIdentifiers.`a-down-task` -> 1
        )
      )
      test("pair") - check(pair)(
        _.down,
        Seq(_.up, _.down),
        Seq(pair.up -> 1, pair.down -> 1)
      )
      test("anonTriple") - check(anonTriple)(
        _.down,
        Seq(_.up, _.down),
        Seq(anonTriple.up -> 1, anonTriple.down -> 2)
      )
      test("diamond") - check(diamond)(
        _.down,
        Seq(_.up, _.left, _.right, _.down),
        Seq(
          diamond.up -> 1,
          diamond.left -> 1,
          diamond.right -> 1,
          diamond.down -> 1
        )
      )

      test("anonDiamond") - check(anonDiamond)(
        _.down,
        Seq(_.down, _.up),
        Seq(
          anonDiamond.up -> 1,
          anonDiamond.down -> 3
        )
      )
    }
    test("multiTerminalGroupCounts") {
      def countGroups(goals: Task[?]*) = {

        val plan = PlanImpl.plan0(goals.map(UnresolvedTask(_, Map.empty)))

        val grouped = PlanImpl.groupAroundImportantTasks(plan.topoSorted, plan.inputs(_)) {
          case ResolvedTask(t: Task.Named[Any], _) => t
          case t if goals.contains(t.task) => t.task
        }
        grouped.keyCount
      }

      test("separateGroups") {
        import separateGroups._
        val groupCount = countGroups(right, left)
        assert(groupCount == 2)
      }

      test("triangleTask") {
        // Make sure the following graph ends up as a single group, since although
        // `right` depends on `left`, both of them depend on the un-cached `task`
        // which would force them both to re-compute every time `task` changes
        import triangleTask._
        val groupCount = countGroups(right, left)
        assert(groupCount == 2)
      }

      test("multiTerminalGroup") {
        // Make sure the following graph ends up as two groups
        import multiTerminalGroup._
        val groupCount = countGroups(right, left)
        assert(groupCount == 2)
      }

      test("multiTerminalBoundary") {
        // Make sure the following graph ends up as three groups: one for
        // each cached task, and one for the downstream task we are running
        import multiTerminalBoundary._
        val groupCount = countGroups(task2)
        assert(groupCount == 3)
      }
    }

  }
}
