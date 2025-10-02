package mill.api

import scala.collection.immutable.ListMap

private[mill] class Plan(
    val transitive: IndexedSeq[ResolvedTask[?]],
    val sortedGroups: MultiBiMap[ResolvedTask[?], ResolvedTask[?]],
    val goalsMap: ListMap[UnresolvedTask[?], Seq[ResolvedTask[?]]],
    val inputs: Map[ResolvedTask[?], Seq[ResolvedTask[?]]],
    val topoSorted: TopoSorted[ResolvedTask[?]],
    val ignoredDetails: Seq[String]
) {
  val goals: Seq[ResolvedTask[?]] = goalsMap.valuesIterator.flatten.toSeq
}
