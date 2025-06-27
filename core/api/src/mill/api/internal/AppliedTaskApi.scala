package mill.api.internal

trait AppliedTaskApi[+T] {
  def apply(): T

  def task: TaskApi[T]
  def crossValues: Map[String, Any]

  def displayName: String
}
