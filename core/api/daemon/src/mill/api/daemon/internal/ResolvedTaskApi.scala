package mill.api.daemon.internal

trait ResolvedTaskApi[+T] {
  def task: TaskApi[T]
  def crossValues: Map[String, String]

  def displayName: String
}
