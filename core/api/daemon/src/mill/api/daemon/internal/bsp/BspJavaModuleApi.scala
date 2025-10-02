package mill.api.daemon.internal.bsp

import mill.api.daemon.internal.{EvaluatorApi, ModuleApi, TaskApi}

import java.nio.file.Path

trait BspJavaModuleApi extends ModuleApi {

  private[mill] def bspBuildTargetInverseSources[T](id: T, uri: String): TaskApi[Seq[T]]

  private[mill] def bspBuildTargetDependencySources
      : TaskApi[(
          resolvedDepsSources: Seq[java.nio.file.Path],
          unmanagedClasspath: Seq[java.nio.file.Path]
      )]

  private[mill] def bspBuildTargetDependencyModules
      : TaskApi[(
          mvnDeps: Seq[(String, String, String)],
          unmanagedClasspath: Seq[java.nio.file.Path]
      )]

  private[mill] def bspBuildTargetSources
      : TaskApi[(
          sources: Seq[java.nio.file.Path],
          generatedSources: Seq[java.nio.file.Path]
      )]

  private[mill] def bspBuildTargetResources: TaskApi[Seq[java.nio.file.Path]]

  private[mill] def bspBuildTargetJavacOptions(
      clientWantsSemanticDb: Boolean,
      crossValues: Map[String, String]
  )
      : TaskApi[EvaluatorApi => (
          classesPath: Path,
          javacOptions: Seq[String],
          classpath: Seq[String]
      )]

  private[mill] def bspBuildTargetScalacOptions(
      enableJvmCompileClasspathProvider: Boolean,
      clientWantsSemanticDb: Boolean,
      crossValues: Map[String, String]
  ): TaskApi[(Seq[String], EvaluatorApi => Seq[String], EvaluatorApi => java.nio.file.Path)]

  private[mill] def bspBuildTargetScalaMainClasses
      : TaskApi[(
          classes: Seq[String],
          forkArgs: Seq[String],
          forkEnv: Map[String, String]
      )]

  private[mill] def bspLoggingTest: TaskApi[Unit]

}
