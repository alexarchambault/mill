package mill.api.daemon.internal

trait UnresolvedTaskApi[+T] {
  def task: TaskApi[T]
  def crossValues: Map[String, String]
}
