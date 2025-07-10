package mill.api

import mill.api.daemon.internal.UnresolvedTaskApi
import mill.api.daemon.internal.TaskApi

final case class UnresolvedTask[+T](task: Task[T], crossValues: Map[String, String])
    extends UnresolvedTaskApi[T]

object UnresolvedTask {
  def apply[T](task: TaskApi[T], crossValues: Map[String, String]): UnresolvedTask[T] =
    task match {
      case task0: Task[T] => UnresolvedTask(task0, crossValues)
      case _ => sys.error(s"Unexpected TaskApi instance type: $task")
    }
}
