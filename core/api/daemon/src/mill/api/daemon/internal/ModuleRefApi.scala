package mill.api.daemon.internal

trait ModuleRefApi[+M <: ModuleApi] {
  def apply(): M
  def crossValues: Map[String, String]

  def taskApi[A](select: M => TaskApi[A]): TaskApi[A] =
    select(apply()).cross(crossValues.toSeq*)
}

object ModuleRefApi {
  def unapply[M <: ModuleApi](ref: ModuleRefApi[M]): (M, Map[String, String]) =
    (ref(), ref.crossValues)

  def equals(first: ModuleRefApi[?], second: ModuleRefApi[?]): Boolean =
    first() == second() &&
      first.crossValues == second.crossValues

  def hashCode(ref: ModuleRefApi[?]): Int =
    ("ModuleRefApi", ref(), ref.crossValues).##
}
