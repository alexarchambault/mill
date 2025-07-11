package mill.api

import mill.api.daemon.internal.ResolvedTaskApi

final case class ResolvedTask[+T](task: Task[T], crossValues: Map[String, String])
    extends ResolvedTaskApi[T] {

  def apply(): T = ???

  def asNamed: Option[ResolvedNamedTask[T]] = task match {
    case n: Task.Named[T] => Some(ResolvedNamedTask(n, crossValues))
    case _ => None
  }

  def asUnappliedTask: UnresolvedTask[T] =
    UnresolvedTask(task, crossValues)

  def displayName: String =
    ResolvedTask.displayName(task, crossValues)
}

object ResolvedTask {
  private[mill] def displayName(task: Task[?], crossValues: Map[String, String]): String =
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
