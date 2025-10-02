package mill.api.daemon.internal

import mill.api.daemon.internal.bsp.BspJavaModuleApi
import mill.api.daemon.internal.eclipse.GenEclipseInternalApi
import mill.api.daemon.internal.idea.{GenIdeaInternalApi, GenIdeaModuleApi}
import mill.api.daemon.internal.{EvaluatorApi, ModuleApi, TaskApi}

trait JavaModuleApi extends ModuleApi with GenIdeaModuleApi {

  def recursiveModuleDeps: Seq[JavaModuleApi | ModuleRefApi[JavaModuleApi]]

  def compileModuleDepsChecked: Seq[JavaModuleApi | ModuleRefApi[JavaModuleApi]]

  def transitiveModuleCompileModuleDeps: Seq[JavaModuleApi | ModuleRefApi[JavaModuleApi]]

  def javacOptions: TaskApi[Seq[String]]
  def mandatoryJavacOptions: TaskApi[Seq[String]]

  // BSP Tasks that sometimes need to be customized

  private[mill] def bspBuildTargetCompile: TaskApi[java.nio.file.Path]

  private[mill] def bspCompileClasspath(
      crossValues: Map[String, String]
  ): TaskApi[EvaluatorApi => Seq[String]]

  /**
   * Internal access to some BSP helper tasks
   */
  private[mill] def bspJavaModule: () => BspJavaModuleApi

  /**
   * Internal access to some GenIdea helper tasks
   */
  private[mill] def genIdeaInternal: () => GenIdeaInternalApi

  /**
   *  Internal access to some GenEclipse helper tasks. These are used when constructing the necessary information to
   *  create a resolved module. This in turn will be used later for creating the actual Eclipse JDT project files!
   */
  private[mill] def genEclipseInternal: () => GenEclipseInternalApi = null
}

object JavaModuleApi
