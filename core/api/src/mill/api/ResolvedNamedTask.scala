package mill.api

import mill.api.daemon.internal.ResolvedTaskApi

final case class ResolvedNamedTask[+T](task: Task.Named[T], crossValues: Map[String, String])
    extends ResolvedTaskApi[T] {

  def apply(): T = ???

  def asTask: ResolvedTask[T] =
    ResolvedTask(task, crossValues)

  def displayName: String =
    ResolvedTask.displayName(task, crossValues)
}
