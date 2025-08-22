package mill.launcher

import coursier.{Artifacts, Dependency, Fetch, ModuleName, Organization, Resolve, VersionConstraint}
import coursier.cache.{ArchiveCache, FileCache}
import coursier.core.Module
import coursier.jvm.{JavaHome, JvmCache, JvmChannel, JvmIndex}
import coursier.launcher.{BootstrapGenerator, ClassLoaderContent, ClassPathEntry, Parameters}
import coursier.maven.MavenRepository
import coursier.util.Task

import java.io.File
import java.util.zip.ZipFile

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Using

object CoursierClient {
  def millDaemonLauncher(isServer: Boolean): java.nio.file.Path = {
    val coursierCache0 = FileCache[Task]()
      .withLogger(coursier.cache.loggers.RefreshLogger.create())

    val testOverridesRepos = Option(System.getenv("MILL_LOCAL_TEST_REPO"))
      .toSeq
      .flatMap(_.split(File.pathSeparator).toSeq)
      .map { path =>
        MavenRepository(os.Path(path).toURI.toASCIIString)
      }

    val artifactsResultOrError = Fetch()
      .withCache(coursierCache0)
      .withDependencies(Seq(Dependency(
        Module(Organization("com.lihaoyi"), ModuleName("mill-runner-daemon_3"), Map()),
        VersionConstraint(mill.client.BuildInfo.millVersion)
      )))
      .withRepositories(testOverridesRepos ++ Resolve.defaultRepositories)
      .eitherResult()
      .right.get

    val cp = artifactsResultOrError.artifacts.map(_._2).map(os.Path(_))
    val compilerInterfaceDeps = artifactsResultOrError.resolution.orderedDependencies
      .iterator
      .filter { dep =>
        dep.module.organization.value == "org.scala-sbt" && (
          dep.module.name.value == "compiler-interface" ||
            dep.module.name.value == "test-interface"
        )
      }
      .toSeq
    val coreApiDeps = artifactsResultOrError.resolution.orderedDependencies
      .iterator
      .filter { dep =>
        val isCoreApi =
          dep.module.organization.value == "com.lihaoyi" &&
            dep.module.name.value == "mill-core-api_3"
        def isCoursierPaths =
          dep.module.organization.value == "io.get-coursier" &&
            dep.module.name.value == "coursier-paths"
        isCoreApi || isCoursierPaths
      }
      .toSeq

    def artifactsFor(deps: Seq[Dependency]) = {
      val subRes =
        artifactsResultOrError.resolution.subset0(deps).right.get
      Artifacts()
        .withResolution(subRes)
        .eitherResult()
        .right.get
        .artifacts
        .map(_._2)
        .map(os.Path(_))
    }
    val interfaceArtifacts = artifactsFor(compilerInterfaceDeps)
    val coreApiArtifacts = artifactsFor(compilerInterfaceDeps ++ coreApiDeps)

    val params = Parameters.Bootstrap(
      Seq(
        // putting compiler-interface and test-interface in a first loader
        // (used to interface with scalac instances and test frameworks)
        ClassLoaderContent(interfaceArtifacts.map(f =>
          ClassPathEntry.Url(f.toNIO.toUri.toASCIIString)
        )),
        // putting core-api right above (used to load builds)
        ClassLoaderContent(coreApiArtifacts.filterNot(interfaceArtifacts.toSet).map(f =>
          ClassPathEntry.Url(f.toNIO.toUri.toASCIIString)
        )),
        // lastly, the remaning JARs
        ClassLoaderContent(cp.filterNot(coreApiArtifacts.toSet).map(f =>
          ClassPathEntry.Url(f.toNIO.toUri.toASCIIString)
        ))
      ),
      mainClass = if (isServer) "mill.daemon.MillDaemonMain" else "mill.daemon.MillNoDaemonMain"
    )
    val launcher = os.temp(Array.emptyByteArray, prefix = "mill-", suffix = ".jar")
    BootstrapGenerator.generate(params, launcher.toNIO)

    // Ensure the modified time of the launcher is deterministic, which helps for
    // cached results (in)validation
    val mtime = cp.map(os.mtime).max
    os.mtime.set(launcher, mtime)

    launcher.toNIO
  }

  def launcherEntries(launcher: java.nio.file.Path): Array[java.nio.file.Path] =
    Using.resource(new ZipFile(launcher.toFile)) { zf =>
      Iterator.from(0)
        .map(n => if (n == 0) "" else s"-$n")
        .map(suffix => s"coursier/bootstrap/launcher/bootstrap-jar-urls$suffix")
        .map(name => zf.getEntry(name))
        .takeWhile(_ != null)
        .map { entry =>
          Using.resource(zf.getInputStream(entry)) { is =>
            new String(is.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
          }
        }
        .flatMap(_.linesIterator)
        .filter(_.nonEmpty)
        .map(new java.net.URI(_))
        .filter(_.getScheme == "file")
        .map(java.nio.file.Paths.get(_))
        .toArray
    }

  def resolveJavaHome(id: String): java.io.File = {
    val coursierCache0 = FileCache[Task]()
      .withLogger(coursier.cache.loggers.RefreshLogger.create())
    val jvmCache = JvmCache()
      .withArchiveCache(ArchiveCache().withCache(coursierCache0))
      .withIndex(
        JvmIndex.load(
          cache = coursierCache0,
          repositories = Resolve.defaultRepositories,
          indexChannel = JvmChannel.module(
            JvmChannel.centralModule(),
            version = mill.client.Versions.coursierJvmIndexVersion
          )
        )
      )

    val javaHome = JavaHome().withCache(jvmCache)
      // when given a version like "17", always pick highest version in the index
      // rather than the highest already on disk
      .withUpdate(true)

    coursierCache0.logger.using(javaHome.get(id)).unsafeRun()(using coursierCache0.ec)
  }
}
