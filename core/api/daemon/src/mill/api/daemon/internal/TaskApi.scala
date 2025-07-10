package mill.api.daemon.internal

trait TaskApi[+T] {
  def apply(): T
  def unresolved(crossValues: Map[String, String]): UnresolvedTaskApi[T]
}
