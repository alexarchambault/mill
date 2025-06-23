package mill.define

import mill.api.{ExecResult, Result, Val}
import mill.define.Plan0.AppliedNamedTask
import mill.define.Plan0.AppliedTask
private[mill] trait SelectiveExecution {
  import SelectiveExecution.*
  def computeHashCodeSignatures(
      transitiveNamed: Seq[AppliedNamedTask[?]],
      codeSignatures: Map[String, Int]
  ): Map[String, Int]

  def computeDownstream(
      plan: Plan0,
      transitiveNamed: Seq[AppliedNamedTask[?]],
      oldHashes: Metadata,
      newHashes: Metadata
  ): (Set[AppliedTask[?]], Seq[AppliedTask[Any]])

  def saveMetadata(metadata: SelectiveExecution.Metadata): Unit

  def computeChangedTasks(
      tasks: Seq[String],
      rootCrossValues: Map[String, Any]
  ): Result[ChangedTasks]

  def computeChangedTasks0(
      tasks: Seq[Task.Named[?]],
      rootCrossValues: Map[String, Any],
      computedMetadata: SelectiveExecution.Metadata.Computed
  ): Option[ChangedTasks]

  def resolve0(tasks: Seq[String]): Result[Array[String]]

  def resolveChanged(tasks: Seq[String]): Result[Seq[String]]

  def resolveTree(tasks: Seq[String]): Result[ujson.Value]

  def computeMetadata(
      tasks: Seq[Task.Named[?]],
      rootCrossValues: Map[String, Any]
  ): SelectiveExecution.Metadata.Computed
}
object SelectiveExecution {
  case class Metadata(inputHashes: Map[String, Int], codeSignatures: Map[String, Int])
  object Metadata {
    case class Computed(
        metadata: Metadata,
        results: Map[AppliedTask[?], ExecResult[Val]]
    )
  }

  implicit val rw: upickle.default.ReadWriter[Metadata] = upickle.default.macroRW

  case class ChangedTasks(
      resolved: Seq[AppliedNamedTask[?]],
      changedRootTasks: Set[AppliedNamedTask[?]],
      downstreamTasks: Seq[AppliedNamedTask[?]]
  )
  object ChangedTasks {

    /** Indicates that all of the passed in tasks were changed. */
    def all(tasks: Seq[AppliedNamedTask[?]]): ChangedTasks = ChangedTasks(tasks, tasks.toSet, tasks)
  }
}
