package mill.api

import mill.api.{ExecResult, Result, Val}

private[mill] trait SelectiveExecution {
  import SelectiveExecution.*
  def computeHashCodeSignatures(
      transitiveNamed: Seq[ResolvedNamedTask[?]],
      codeSignatures: Map[String, Int]
  ): Map[String, Int]

  def computeDownstream(
      plan: Plan,
      transitiveNamed: Seq[ResolvedNamedTask[?]],
      oldHashes: Metadata,
      newHashes: Metadata
  ): (Set[ResolvedTask[?]], Seq[ResolvedTask[Any]])

  def saveMetadata(metadata: SelectiveExecution.Metadata): Unit

  def computeChangedTasks(
      tasks: Seq[String],
      rootCrossValues: Map[String, String]
  ): Result[ChangedTasks]

  def computeChangedTasks0(
      tasks: Seq[UnresolvedTask[?]],
      computedMetadata: SelectiveExecution.Metadata.Computed
  ): Option[Result[ChangedTasks]]

  def resolve0(tasks: Seq[String]): Result[Array[String]]

  def resolveChanged(tasks: Seq[String]): Result[Seq[String]]

  def resolveTree(tasks: Seq[String]): Result[ujson.Value]

  def computeMetadata(
      tasks: Seq[ResolvedNamedTask[?]]
  ): mill.api.Result[SelectiveExecution.Metadata.Computed]
}
object SelectiveExecution {
  case class Metadata(inputHashes: Map[String, Int], codeSignatures: Map[String, Int])
  object Metadata {
    case class Computed(
        metadata: Metadata,
        results: Map[ResolvedTask[?], ExecResult[Val]]
    )
  }

  implicit val rw: upickle.ReadWriter[Metadata] = upickle.macroRW

  case class ChangedTasks(
      resolved: Seq[ResolvedNamedTask[?]],
      changedRootTasks: Set[ResolvedNamedTask[?]],
      downstreamTasks: Seq[ResolvedNamedTask[?]]
  )
  object ChangedTasks {

    /** Indicates that all of the passed in tasks were changed. */
    def all(tasks: Seq[ResolvedNamedTask[?]]): ChangedTasks =
      ChangedTasks(tasks, tasks.toSet, tasks)
  }
}
