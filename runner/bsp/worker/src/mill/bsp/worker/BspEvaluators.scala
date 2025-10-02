package mill.bsp.worker

import ch.epfl.scala.bsp4j.BuildTargetIdentifier
import mill.api.daemon.internal.bsp.BspModuleApi
import mill.api.daemon.internal.{BaseModuleApi, EvaluatorApi, ModuleApi, ModuleRefApi}
import mill.api.daemon.Watchable

private[mill] class BspEvaluators(
    workspaceDir: os.Path,
    val evaluators: Seq[EvaluatorApi],
    debug: (() => String) => Unit,
    val watched: Seq[Watchable]
) {

  /**
   * Compute all transitive modules from module children and via moduleDeps + compileModuleDeps
   */
  def transitiveModules(module: ModuleApi): Seq[ModuleApi] = {
    Seq(module) ++ module.moduleDirectChildren.flatMap(transitiveModules)
  }

  lazy val bspModulesIdList
      : Seq[(BuildTargetIdentifier, (ModuleRefApi[BspModuleApi], EvaluatorApi))] = {
    val modules: Seq[(ModuleApi, Seq[ModuleApi], EvaluatorApi)] = evaluators
      .map(ev => (ev.rootModule, transitiveModules(ev.rootModule), ev))

    modules
      .flatMap { case (rootModule, modules, eval) =>
        modules.collect {
          case m: BspModuleApi =>
            val crossValues = m.bspCrossValueTasks.map { task =>
              val allCrossValues = eval.resolvedTasks(task.unresolved(Map.empty))
                .toOption
                .toSeq
                .flatten
                .map(_.crossValues)
              assert(allCrossValues.isEmpty || allCrossValues.map(_.keySet).toSet.size == 1)
              task -> allCrossValues
            }

            val allKeys = crossValues.foldLeft(
              Set.empty[String]
            )(_ ++ _._2.headOption.map(_.keySet).getOrElse(Set.empty))
            pprint.err.log(allKeys)
            val retainedKeyValues = allKeys.toSeq
              .map { key =>
                val valuesPerTask = crossValues.flatMap {
                  case (task, l) =>
                    val valuesOpt =
                      if (l.headOption.exists(_.contains(key))) Some(l.map(_(key)).toSet)
                      else None
                    valuesOpt.map(task -> _).toSeq
                }
                val allValues = valuesPerTask.map(_._2).foldLeft(Set.empty[String])(_ ++ _)
                val valuesWithMissingTasks = allValues.toSeq
                  .map { value =>
                    value -> valuesPerTask.filter(!_._2.contains(value)).map(_._1)
                  }
                  .filter(_._2.nonEmpty)
                if (valuesWithMissingTasks.nonEmpty) {
                  // TODO Warn about it
                  pprint.err.log(valuesWithMissingTasks)
                }
                key -> allValues.filterNot(valuesWithMissingTasks.map(_._1).toSet)
              }
              .toMap
            pprint.err.log(retainedKeyValues)

            val willBeEmptyBecauseOf = retainedKeyValues.filter(_._2.isEmpty)
            if (willBeEmptyBecauseOf.nonEmpty) {
              // TODO Warn about it
              pprint.err.log(willBeEmptyBecauseOf)
            }

            val retainedKeyValues0 = retainedKeyValues
              .iterator
              .map {
                case (key, values) =>
                  (key, values.toList.sorted)
              }
              .toList
              .sortBy(_._1)

            def enumerate(
                keyValues: List[(String, List[String])],
                acc: List[List[(String, String)]]
            ): List[List[(String, String)]] =
              keyValues match {
                case Nil => acc
                case (key, values) :: rem =>
                  val newAcc =
                    for {
                      value <- values
                      elem <- acc
                    } yield (key, value) :: elem
                  enumerate(rem, newAcc)
              }

            val enumerated = enumerate(retainedKeyValues0, List(Nil))

            if (enumerated.isEmpty) {
              // TODO Warn about this, but should only happen when willBeEmptyBecauseOf.nonEmpty (see above)
              pprint.err.log(enumerated)
            }

            val uri = Utils.sanitizeUri(
              (os.Path(rootModule.moduleDirJava) / m.moduleSegments.parts).toNIO
            )

            enumerated.map { entries =>
              val uri0 = uri + Utils.encodeQuery(entries)
              (
                new BuildTargetIdentifier(uri0),
                (BspEvaluators.ModuleRefImpl(m, entries.toMap), eval)
              )
            }

          // (new BuildTargetIdentifier(uri), (BspEvaluators.ModuleRefImpl(m, Map.empty), eval))
        }.flatten
      }
  }
  lazy val bspModulesById
      : Map[BuildTargetIdentifier, (ModuleRefApi[BspModuleApi], EvaluatorApi)] = {
    val map = bspModulesIdList.toMap
    debug(() => s"BspModules: ${map.view.mapValues(_._1().bspDisplayName).toMap}")
    map
  }

  lazy val rootModules: Seq[BaseModuleApi] = evaluators.map(_.rootModule)

  lazy val bspIdByModule: Map[ModuleRefApi[BspModuleApi], BuildTargetIdentifier] =
    bspModulesById.view.mapValues(_._1).map(_.swap).toMap
  lazy val syntheticRootBspBuildTarget: Option[SyntheticRootBspBuildTargetData] =
    SyntheticRootBspBuildTargetData.makeIfNeeded(bspModulesById.values.map(_._1), workspaceDir)

  def filterNonSynthetic(input: java.util.List[BuildTargetIdentifier])
      : java.util.List[BuildTargetIdentifier] = {
    import scala.jdk.CollectionConverters.*
    input.asScala.filterNot(syntheticRootBspBuildTarget.map(_.id).contains).toList.asJava
  }
}

object BspEvaluators {
  private[mill] final case class ModuleRefImpl[+M <: ModuleApi](
      module: M,
      crossValues: Map[String, String]
  ) extends ModuleRefApi[M] {
    def apply(): M = module
    override def equals(that: Any): Boolean =
      that.isInstanceOf[ModuleRefApi[?]] &&
        ModuleRefApi.equals(this, that.asInstanceOf[ModuleRefApi[?]])
    override def hashCode(): Int =
      ModuleRefApi.hashCode(this)
  }
}
