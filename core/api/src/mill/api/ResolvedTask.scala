package mill.api

import mill.api.daemon.internal.ResolvedTaskApi

abstract class ResolvedTask[+T] extends ResolvedTaskApi[T] {
  def task: Task[T]
  def crossValues: Map[String, String]

  def asNamed: Option[ResolvedNamedTask[T]] =
    this match {
      case named: ResolvedNamedTask[?] => Some(named)
      case _ =>
        task match {
          case n: Task.Named[T] => Some(n.resolved(crossValues))
          case _ => None
        }
    }

  def asUnappliedTask: UnresolvedTask[T] =
    UnresolvedTask(task, crossValues)

  def displayName: String =
    ResolvedTask.displayName(task, crossValues)
}

object ResolvedTask {
  private final case class Impl[T](task: Task[T], crossValues: Map[String, String])
      extends ResolvedTask[T]

  def apply[T](task: Task[T], crossValues: Map[String, String]): ResolvedTask[T] =
    Impl(task, crossValues)

  def unapply[T](resolved: ResolvedTask[T]): (task: Task[T], crossValues: Map[String, String]) =
    (resolved.task, resolved.crossValues)

  private[mill] def displayName(task: Task[?], crossValues: Map[String, String]): String =
    displayName(task.toString, crossValues)
  private[mill] def displayName(task: String, crossValues: Map[String, String]): String =
    task + {
      if (crossValues.isEmpty) ""
      else
        crossValues
          .toSeq
          .sortBy(_._1)
          .map {
            case (k, v) =>
              s",$k=$v"
          }
          .mkString
    }
}
