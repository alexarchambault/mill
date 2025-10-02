package mill.api

private[mill] class Plan(
    val transitive: IndexedSeq[ResolvedTask[?]],
    val sortedGroups: MultiBiMap[ResolvedTask[?], ResolvedTask[?]],
    val goals: Seq[ResolvedTask[?]],
    val inputs: Map[ResolvedTask[?], Seq[ResolvedTask[?]]],
    val topoSorted: TopoSorted[ResolvedTask[?]],
    val ignoredDetails: Seq[String]
)
