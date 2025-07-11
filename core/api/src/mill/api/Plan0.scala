package mill.api

private[mill] class Plan0(
    val transitive: IndexedSeq[ResolvedTask[?]],
    val sortedGroups: MultiBiMap[ResolvedTask[?], ResolvedTask[?]],
    val goals: Seq[ResolvedTask[?]],
    val inputs: Map[ResolvedTask[?], Seq[ResolvedTask[?]]],
    val topoSorted: TopoSorted[ResolvedTask[?]]
)
