package mill.exec

import mill.api.Task

private final case class ExecutionTask[+T](
    task: Task[T],
    availableCrossValues: Map[String, String],
    appliedCrossValues: Map[String, String]
)
