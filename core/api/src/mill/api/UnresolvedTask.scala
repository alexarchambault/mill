package mill.api

import mill.api.daemon.internal.UnresolvedTaskApi

sealed abstract class UnresolvedTask[+T]
    extends UnresolvedTaskApi[T] {

  def task: Task[T]
  def crossValues: Map[String, String]

  def displayName: String =
    ResolvedTask.displayName(task, crossValues)
}

object UnresolvedTask {

  def apply[T](task: Task[T], crossValues: Map[String, String]): UnresolvedTask[T] =
    Impl(task, crossValues)

  private final case class Impl[+T](task: Task[T], crossValues: Map[String, String])
      extends UnresolvedTask[T]

  final case class Named[+T](task: Task.Named[T], crossValues: Map[String, String])
      extends UnresolvedTask[T] {
    def asTask: UnresolvedTask[T] = UnresolvedTask(task, crossValues)
    def resolved(): ResolvedNamedTask[T] =
      ResolvedNamedTask(task, crossValues)
    def as[U]: Named[U] =
      this.asInstanceOf[Named[U]]
    def render: String =
      task.ctx.segments.withCrossValues(crossValues.toSeq).render
    def asSimpleTask: Task[T] =
      task.cross(crossValues.toSeq*)
  }
}
