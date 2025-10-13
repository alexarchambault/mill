package mill.api

final case class ResolvedNamedTask[+T](task: Task.Named[T], crossValues: Map[String, String])
    extends ResolvedTask[T] {

  def asTask: ResolvedTask[T] =
    ResolvedTask(task, crossValues)
  def render: String =
    task.ctx.segments.withCrossValues(crossValues.toSeq).render
}
