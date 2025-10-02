package mill.api.daemon.internal.bsp

import mill.api.daemon.internal.{ModuleApi, TaskApi}

trait BspModuleApi extends ModuleApi {
  private[mill] def bspBuildTargetData: TaskApi[Option[(String, AnyRef)]]
  private[mill] def bspBuildTarget: BspBuildTarget

  // FIXME Cross-values aren't taken into account when used most of the time
  private[mill] def bspDisplayName: String

  def bspCrossValueTasks: Seq[TaskApi[?]]
}

object BspModuleApi {

  /** Used to define the [[BspBuildTarget.languageIds]] field. */
  object LanguageId {
    val Java = "java"
    val Scala = "scala"
    val Kotlin = "kotlin"
  }

  /** Used to define the [[BspBuildTarget.tags]] field. */
  object Tag {
    val Library = "library"
    val Application = "application"
    val Test = "test"
    val IntegrationTest = "integration-test"
    val Benchmark = "benchmark"
    val NoIDE = "no-ide"
    val Manual = "manual"
  }
}
