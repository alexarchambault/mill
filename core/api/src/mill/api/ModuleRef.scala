package mill.api

/**
 * Used to refer to a module from another module without including the target
 * module as a child-module of the first.
 */
final case class ModuleRef[+T <: mill.api.Module](
    t: T,
    crossValues: Map[String, String] = Map.empty
) extends mill.api.daemon.internal.ModuleRefApi[T] {
  def apply(): T = t

  def addCrossValues(extraValues: Seq[(String, String)]): ModuleRef[T] =
    // order is important - we don't override existing values
    copy(crossValues = extraValues.toMap ++ crossValues)

  def unresolvedTask[A](select: T => Task[A]): UnresolvedTask[A] =
    UnresolvedTask(select(t), crossValues)
  def task[A](select: T => Task[A]): Task[A] =
    select(t).cross(crossValues.toSeq*)
  def moduleRef[M <: mill.api.Module](select: T => ModuleRef[M]): ModuleRef[M] =
    select(t).addCrossValues(crossValues.toSeq)

  def displayName: String =
    if (crossValues.isEmpty) t.toString
    else
      t.toString +
        crossValues
          .toVector
          .sorted
          .map { case (k, v) => s"$k=$v" }
          .mkString(" (", ", ", ")")
}
