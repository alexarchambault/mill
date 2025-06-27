package mill.define

import mill.api.internal.AppliedTaskApi

private[mill] class Plan0(
    val transitive: IndexedSeq[Plan0.AppliedTask[?]],
    val sortedGroups: MultiBiMap[Plan0.AppliedTask[?], Plan0.AppliedTask[?]],
    val goals: Seq[Plan0.AppliedTask[?]],
    val inputs: Map[Plan0.AppliedTask[?], Seq[Plan0.AppliedTask[?]]]
)

object Plan0 {
  final case class UnappliedTask[+T](task: Task[T], crossValues: Map[String, String])

  final case class AppliedTask[+T](task: Task[T], crossValues: Map[String, String])
      extends AppliedTaskApi[T] {

    def apply(): T = ???

    def asNamed: Option[AppliedNamedTask[T]] = task match {
      case n: Task.Named[T] => Some(AppliedNamedTask(n, crossValues))
      case _ => None
    }

    def asUnappliedTask: UnappliedTask[T] =
      UnappliedTask(task, crossValues)

    def displayName: String =
      Plan0.displayName(task, crossValues)
  }

  final case class AppliedNamedTask[+T](task: Task.Named[T], crossValues: Map[String, String])
      extends AppliedTaskApi[T] {

    def apply(): T = ???

    def asTask: AppliedTask[T] =
      AppliedTask(task, crossValues)

    def displayName: String =
      Plan0.displayName(task, crossValues)
  }

  private def displayName(task: Task[?], crossValues: Map[String, String]): String =
    task.toString + {
      if (crossValues.isEmpty) ""
      else
        crossValues
          .toSeq
          .sortBy(_._1)
          .map {
            case (k, v) =>
              s"$k=$v"
          }
          .mkString(" (", ", ", ")")
    }
}
