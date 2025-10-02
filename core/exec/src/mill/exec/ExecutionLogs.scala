package mill.exec

import mill.constants.OutFiles
import mill.internal.SpanningForest

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.EnumerationHasAsScala

private object ExecutionLogs {
  def logDependencyTree[T](
      interGroupDeps: Map[T, Seq[T]],
      indexToTerminal: Array[T],
      outPath: os.Path,
      render: T => String
  ): Unit = {
    val ( /*vertexToIndex*/ _, edgeIndices) =
      SpanningForest.graphMapToIndices(indexToTerminal, interGroupDeps)

    SpanningForest.writeJsonFile(
      outPath / OutFiles.millDependencyTree,
      edgeIndices,
      indexToTerminal.indices.toSet,
      t => render(indexToTerminal(t))
    )
  }
  def logInvalidationTree[T](
      interGroupDeps: Map[T, Seq[T]],
      indexToTerminal: Array[T],
      outPath: os.Path,
      uncached: ConcurrentHashMap[T, Unit],
      changedValueHash: ConcurrentHashMap[T, Unit],
      render: T => String,
      drop: T => Boolean
  ): Unit = {
    val reverseInterGroupDeps = SpanningForest.reverseEdges(interGroupDeps)

    val changedTerminalIndices = changedValueHash.keys().asScala.toSet

    val (vertexToIndex, downstreamIndexEdges) = SpanningForest.graphMapToIndices(
      indexToTerminal,
      reverseInterGroupDeps.view.filterKeys(changedTerminalIndices).toMap
    )

    val edgeSourceIndices = downstreamIndexEdges
      .zipWithIndex
      .collect { case (es, i) if es.nonEmpty => i }
      .toSet

    SpanningForest.writeJsonFile(
      outPath / OutFiles.millInvalidationTree,
      downstreamIndexEdges,
      uncached.keys().asScala
        .flatMap { uncachedTask =>
          val uncachedIndex = vertexToIndex(uncachedTask)
          Option.when(
            // Filter out input and source tasks which do not cause downstream invalidations
            // from the invalidation tree, because most of them are un-interesting and the
            // user really only cares about (a) inputs that cause downstream tasks to invalidate
            // or (b) non-input tasks that were invalidated alone (e.g. due to a codesig change)
            !drop(uncachedTask) || edgeSourceIndices(uncachedIndex)
          ) {
            uncachedIndex
          }
        }
        .toSet,
      t => render(indexToTerminal(t))
    )
  }
}
