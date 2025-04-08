package mill.scalalib

import mill.*
import mill.constants.Util

import scala.util.Properties

/**
 * Provides a [[NativeImageModule.nativeImage task]] to build a native executable using [[https://www.graalvm.org/ Graal VM]].
 *
 * It is recommended to specify a custom JDK that includes the `native-image` Tool.
 * {{{
 * trait AppModule extends NativeImageModule {
 *   def jvmWorker = ModuleRef(JvmWorkerGraalvm)
 *
 *   object JvmWorkerGraalvm extends JvmWorkerModule {
 *     def jvmId = "graalvm-community:23.0.1"
 *   }
 * }
 * }}}
 */
@mill.api.experimental
trait NativeImageModule extends WithJvmWorker {
  def runClasspath: T[Seq[PathRef]]
  def finalMainClass: T[String]

  /**
   * [[https://www.graalvm.org/latest/reference-manual/native-image/#from-a-class Builds a native executable]] for this
   * module with [[finalMainClass]] as the application entry point.
   */
  def nativeImage: T[PathRef] = Task {
    val dest = Task.dest

    val executeableName = "native-executable"
    val command = Seq.newBuilder[String]
      .+=(nativeImageTool().path.toString)
      .++=(nativeImageOptions())
      .+=("-cp")
      .+=(nativeImageClasspath().iterator.map(_.path).mkString(java.io.File.pathSeparator))
      .+=(finalMainClass())
      .+=((dest / executeableName).toString())
      .result()

    os.proc(command).call(cwd = dest, stdout = os.Inherit)

    val ext = if (Util.isWindows) ".exe" else ""
    val executable = dest / s"$executeableName$ext"
    assert(os.exists(executable))
    PathRef(executable)
  }

  def nativeImageCopyJvm: T[Boolean] =
    if (Properties.isWin)
      Task {
        val home = nativeImageOriginalJavaHome()
        // In https://github.com/graalvm/graalvm-ce-builds/releases/download/jdk-23.0.1/graalvm-community-jdk-23.0.1_windows-x64_bin.zip,
        // the max length of a header file path relative to the JVM root is 56.
        // The paths passed to CL.EXE should be at most 260 in length. So 200 is a reasonable limit.
        home.toString.length >= 200
      }
    else
      Task(false)

  private def nativeImageOriginalJavaHome: Task[os.Path] = Task.Anon {
    jvmWorker().javaHome().map(_.path)
      .orElse(sys.env.get("GRAALVM_HOME").map(os.Path(_))).getOrElse {
        throw new RuntimeException("JvmWorkerModule.javaHome/GRAALVM_HOME not defined")
      }
  }

  def nativeImageJavaHome: Task[PathRef] = Task {
    if (nativeImageCopyJvm()) {
      val dest = T.dest
      os.makeDir.all(dest)
      for (elem <- os.list(nativeImageOriginalJavaHome()))
        os.copy(elem, dest / elem.last)
      PathRef(dest, quick = true)
    } else
      PathRef(nativeImageOriginalJavaHome(), quick = true)
  }

  /**
   * The classpath to use to generate the native image. Defaults to [[runClasspath]].
   */
  def nativeImageClasspath: T[Seq[PathRef]] = Task {
    runClasspath()
  }

  /**
   * Additional options for the `native-image` Tool.
   */
  def nativeImageOptions: T[Seq[String]] = Seq.empty[String]

  /**
   * Path to the [[https://www.graalvm.org/latest/reference-manual/native-image/ `native-image` Tool]].
   * Defaults to a path relative to
   *  - [[JvmWorkerModule.javaHome]], if defined
   *  - environment variable `GRAALVM_HOME`, if defined
   *
   * @note The task fails if the `native-image` Tool is not found.
   */
  def nativeImageTool: T[PathRef] = Task {
    val home = nativeImageJavaHome().path
    val tool = if (Properties.isWin) "native-image.cmd" else "native-image"
    val path = home / "bin" / tool
    if (os.exists(path)) PathRef(path)
    else throw new RuntimeException(s"$path not found")
  }
}
