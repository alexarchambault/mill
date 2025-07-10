package mill.api

/**
 * Represents the topologically sorted set of tasks
 */
final class TopoSorted[T] private[mill] (val values: IndexedSeq[T])
