package mill.api

final case class UnresolvedTask[+T](task: Task[T], crossValues: Map[String, String])
