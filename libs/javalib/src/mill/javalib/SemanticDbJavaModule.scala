package mill.javalib

import mill.api.{BuildCtx, Discover, ExternalModule, ModuleRef, PathRef, Result, experimental}
import mill.api.daemon.internal.SemanticDbJavaModuleApi
import mill.constants.CodeGenConstants
import mill.util.BuildInfo
import mill.javalib.api.{CompilationResult, JvmWorkerUtil}
import mill.util.Version
import mill.{T, Task}

import scala.jdk.CollectionConverters.*
import mill.api.daemon.internal.bsp.BspBuildTarget

@experimental
trait SemanticDbJavaModule extends CoursierModule with SemanticDbJavaModuleApi
    with WithJvmWorkerModule {

  def jvmWorker: ModuleRef[JvmWorkerModule]

  def upstreamSemanticDbDatas: Task[Seq[SemanticDbJavaModule.SemanticDbData]] =
    Task.sequence(transitiveModuleCompileModuleDeps.map {
      case m: SemanticDbJavaModule =>
        m.semanticDbDataDetailed
      case r: ModuleRef[SemanticDbJavaModule] =>
        r.task(_.semanticDbDataDetailed)
    })

  def transitiveModuleCompileModuleDeps: Seq[SemanticDbJavaModule | ModuleRef[SemanticDbJavaModule]]
  def zincReportCachedProblems: T[Boolean]
  def zincIncrementalCompilation: T[Boolean]
  def allSourceFiles: T[Seq[PathRef]]
  def compile: T[mill.javalib.api.CompilationResult]

  private[mill] def compileFor(compileFor: CompileFor): Task[mill.javalib.api.CompilationResult] =
    compileFor match {
      case CompileFor.Regular => compile
      case CompileFor.SemanticDb => semanticDbDataDetailed.map(_.compilationResult)
    }

  private[mill] def bspBuildTarget: BspBuildTarget
  def javacOptions: T[Seq[String]]
  def mandatoryJavacOptions: T[Seq[String]]
  private[mill] def compileClasspathTask(compileFor: CompileFor): Task[Seq[PathRef]]
  def moduleDeps: Seq[JavaModule | ModuleRef[JavaModule]]

  def semanticDbVersion: T[String] = Task.Input {
    val builtin = SemanticDbJavaModuleApi.buildTimeSemanticDbVersion
    val requested = Task.env.getOrElse[String](
      "SEMANTICDB_VERSION",
      SemanticDbJavaModuleApi.contextSemanticDbVersion.get().getOrElse(builtin)
    )
    Version.chooseNewest(requested, builtin)(using Version.IgnoreQualifierOrdering)
  }

  def semanticDbJavaVersion: T[String] = Task.Input {
    val builtin = SemanticDbJavaModuleApi.buildTimeJavaSemanticDbVersion
    val requested = Task.env.getOrElse[String](
      "JAVASEMANTICDB_VERSION",
      SemanticDbJavaModuleApi.contextJavaSemanticDbVersion.get().getOrElse(builtin)
    )
    Version.chooseNewest(requested, builtin)(using Version.IgnoreQualifierOrdering)
  }

  def semanticDbScalaVersion: T[String] = BuildInfo.scalaVersion

  protected def semanticDbPluginMvnDeps: T[Seq[Dep]] = Task {
    val sv = semanticDbScalaVersion()
    val semDbVersion = semanticDbVersion()
    if (!JvmWorkerUtil.isScala3(sv) && semDbVersion.isEmpty) {
      val msg =
        """|
           |With Scala 2 you must provide a semanticDbVersion
           |
           |def semanticDbVersion = ???
           |""".stripMargin
      Task.fail(msg)
    } else if (JvmWorkerUtil.isScala3(sv)) {
      Seq.empty[Dep]
    } else {
      Seq(
        mvn"org.scalameta:semanticdb-scalac_${sv}:${semDbVersion}"
      )
    }
  }

  private def semanticDbJavaPluginMvnDeps: T[Seq[Dep]] = Task {
    val sv = semanticDbJavaVersion()
    if (sv.isEmpty) {
      val msg =
        """|
           |You must provide a javaSemanticDbVersion
           |
           |def semanticDbJavaVersion = ???
           |""".stripMargin
      Task.fail(msg)
    } else {
      Seq(
        mvn"com.sourcegraph:semanticdb-javac:${sv}"
      )
    }
  }

  /**
   * Scalac options to activate the compiler plugins.
   */
  protected def semanticDbEnablePluginScalacOptions: T[Seq[String]] = Task {
    val resolvedJars = defaultResolver().classpath(
      semanticDbPluginMvnDeps().map(_.exclude("*" -> "*"))
    )
    resolvedJars.iterator.map(jar => s"-Xplugin:${jar.path}").toSeq
  }

  protected def semanticDbPluginClasspath: T[Seq[PathRef]] = Task {
    defaultResolver().classpath(semanticDbPluginMvnDeps())
  }

  protected def resolvedSemanticDbJavaPluginMvnDeps: T[Seq[PathRef]] = Task {
    defaultResolver().classpath(semanticDbJavaPluginMvnDeps())
  }

  def semanticDbDataDetailed: T[SemanticDbJavaModule.SemanticDbData] = Task(persistent = true) {
    Task.log.debug(s"effective javac options: ${javacOptions() ++ mandatoryJavacOptions()}")

    val compilationResult = compile()
    val semanticDbFiles = BuildCtx.withFilesystemCheckerDisabled {
      SemanticDbJavaModule.copySemanticdbFiles(
        compilationResult.classes.path,
        BuildCtx.workspaceRoot,
        Task.dest / "data",
        SemanticDbJavaModule.workerClasspath().map(_.path),
        allSourceFiles().map(_.path)
      )
    }

    SemanticDbJavaModule.SemanticDbData(compilationResult, semanticDbFiles)
  }

  def semanticDbData: T[PathRef] = Task {
    semanticDbDataDetailed().semanticDbFiles
  }

  override private[mill] def bspBuildTargetCompileSemanticDb = Task.Anon {
    compile().classes.path.toNIO
  }
}

object SemanticDbJavaModule extends ExternalModule with CoursierModule {
  case class SemanticDbData(
      compilationResult: CompilationResult,
      semanticDbFiles: PathRef
  ) derives upickle.ReadWriter

  private[mill] def workerClasspath: T[Seq[PathRef]] = Task {
    defaultResolver().classpath(Seq(
      Dep.millProjectModule("mill-libs-javalib-scalameta-worker")
    ))
  }

  private val userCodeStartMarker = "//SOURCECODE_ORIGINAL_CODE_START_MARKER"

  private def postProcessed(
      generatedSourceSemdb: os.Path,
      sourceroot: os.Path,
      semdbRoot: os.Path,
      workerClasspath: Seq[os.Path]
  ): Option[(Array[Byte], os.SubPath)] = {
    val baseName = generatedSourceSemdb.last.stripSuffix(".semanticdb")
    val isGenerated = CodeGenConstants.buildFileExtensions.asScala
      .exists(ext => baseName.endsWith("." + ext))
    Option.when(isGenerated) {
      val generatedSourceSubPath = {
        val subPath = generatedSourceSemdb.relativeTo(semdbRoot).asSubPath
        subPath / os.up / subPath.last.stripSuffix(".semanticdb")
      }
      val generatedSource = sourceroot / generatedSourceSubPath
      val generatedSourceLines = os.read.lines(generatedSource)
      val source = generatedSourceLines
        .collectFirst { case s"//SOURCECODE_ORIGINAL_FILE_PATH=$rest" => os.Path(rest.trim) }
        .getOrElse {
          sys.error(s"Cannot get original source from generated source $generatedSource")
        }

      val firstLineIdx = generatedSourceLines.indexWhere(_.startsWith(userCodeStartMarker)) + 1

      val res = mill.util.Jvm.withClassLoader(
        workerClasspath,
        parent = getClass.getClassLoader
      ) { cl =>
        val cls = cl.loadClass("mill.javalib.scalameta.worker.SemanticdbProcessor")
        cls.getMethods.find(_.getName == "postProcess")
          .get
          .invoke(
            null,
            os.read(source),
            source.relativeTo(sourceroot),
            (lineIdx: Int) => Some(lineIdx - firstLineIdx).filter(_ >= 0),
            generatedSourceSemdb
          ).asInstanceOf[Array[Byte]]
      }

      val sourceSemdbSubPath = {
        val sourceSubPath = source.relativeTo(sourceroot).asSubPath
        sourceSubPath / os.up / s"${sourceSubPath.last}.semanticdb"
      }
      (res, sourceSemdbSubPath)
    }
  }

  // The semanticdb-javac plugin has issues with the -sourceroot setting, so we correct this on the fly
  private[mill] def copySemanticdbFiles(
      classesDir: os.Path,
      sourceroot: os.Path,
      targetDir: os.Path,
      workerClasspath: Seq[os.Path],
      sources: Seq[os.Path]
  ): PathRef = {
    assert(classesDir != targetDir)
    os.remove.all(targetDir)
    os.makeDir.all(targetDir)

    val semanticPath = os.rel / "META-INF/semanticdb"
    val toClean = classesDir / semanticPath / sourceroot.segments.toSeq

    val existingSources = sources.map(_.subRelativeTo(sourceroot)).toSet

    // copy over all found semanticdb-files into the target directory
    // but with corrected directory layout
    if (os.exists(classesDir)) {
      for (source <- os.walk(classesDir, preOrder = true) if os.isFile(source)) {
        val dest =
          if (source.startsWith(toClean)) targetDir / semanticPath / source.relativeTo(toClean)
          else targetDir / source.relativeTo(classesDir)

        os.copy(source, dest, createFolders = true)

        // SemanticDB plugin doesn't delete old `.semanticdb` files during incremental compilation.
        // So we need to do it ourselves, using the fact that the `.semanticdb` files have the same
        // name and path as the `.java` or `.scala` files they are created from, just with `.semanticdb`
        // appended on the end
        import os./
        dest match {
          case folder / s"$prefix.semanticdb"
              if !existingSources.contains(
                (folder / prefix).subRelativeTo(targetDir / semanticPath)
              ) =>
            os.remove(dest)
          case _ =>
        }

        postProcessed(source, sourceroot, classesDir / semanticPath, workerClasspath)
          .foreach { case (data, dest) =>
            os.write.over(targetDir / semanticPath / dest, data, createFolders = true)
          }
      }
    }

    PathRef(targetDir)
  }

  // The semanticdb-javac plugin has issues with the -sourceroot setting, so we correct this on the fly
  private[mill] def updateSemanticdbFiles(
      classesDir: os.Path,
      sourceroot: os.Path,
      workerClasspath: Seq[os.Path],
      sources: Seq[os.Path]
  ): Unit = {
    val semanticPath = os.rel / "META-INF/semanticdb"
    val toClean = classesDir / semanticPath / sourceroot.segments.toSeq

    val existingSources = sources.map(_.subRelativeTo(sourceroot)).toSet

    // copy over all found semanticdb-files into the target directory
    // but with corrected directory layout
    if (os.exists(classesDir)) {
      for (source <- os.walk(classesDir, preOrder = true) if os.isFile(source)) {
        val moveToOpt =
          if (source.startsWith(toClean))
            Some(classesDir / semanticPath / source.subRelativeTo(toClean))
          else None

        for (moveTo <- moveToOpt)
          os.move(source, moveTo, createFolders = true)

        val source0 = moveToOpt.getOrElse(source)
        // SemanticDB plugin doesn't delete old `.semanticdb` files during incremental compilation.
        // So we need to do it ourselves, using the fact that the `.semanticdb` files have the same
        // name and path as the `.java` or `.scala` files they are created from, just with `.semanticdb`
        // appended on the end
        import os./
        source0 match {
          case folder / s"$prefix.semanticdb"
              if !existingSources.contains(
                (folder / prefix).subRelativeTo(classesDir / semanticPath)
              ) =>
            os.remove(source0)
          case _ =>
        }

        postProcessed(source, sourceroot, classesDir / semanticPath, workerClasspath)
          .foreach { case (data, dest) =>
            os.write.over(classesDir / semanticPath / dest, data, createFolders = true)
          }
      }
    }
  }

  def copySemanticdbFiles(
      classesDir: os.Path,
      sourceroot: os.Path,
      targetDir: os.Path
  ): PathRef = ??? // bincompat stub

  lazy val millDiscover = Discover[this.type]
}
