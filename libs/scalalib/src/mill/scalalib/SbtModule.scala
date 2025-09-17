package mill.scalalib

import mill.Task
import mill.javalib.api.JvmWorkerUtil.matchingVersions

/**
 * A [[ScalaModule]] with sbt compatible directory layout.
 */
trait SbtModule extends ScalaModule with MavenModule {

  override def sources = {
    val baseDirs = Seq(
      os.sub / "src/main/scala",
      os.sub / "src/main/java"
    )
    scalaVersion match {
      case scalaVersion0: Task.CrossValue[String] =>
        def directories(sv: String): Seq[os.SubPath] =
          baseDirs ++ matchingVersions(sv).map { sv0 =>
            os.sub / "src/main" / s"scala-$sv0"
          }
        Task.Sources(directories(scalaVersion0())*)
      case _ =>
        Task.Sources(baseDirs*)
    }
  }

  trait SbtTests extends ScalaTests with MavenTests {
    override def sources = {
      val baseDirs = Seq(
        moduleDir / "src" / testModuleName / "java",
        moduleDir / "src" / testModuleName / "scala"
      )
      scalaVersion match {
        case scalaVersion0: Task.CrossValue[String] =>
          def directories(sv: String): Seq[os.Path] =
            baseDirs ++ matchingVersions(sv).map { sv0 =>
              moduleDir / "src" / testModuleName / s"scala-$sv0"
            }
          Task.Sources(directories(scalaVersion0())*)
        case _ =>
          Task.Sources(baseDirs*)
      }
    }
  }
}
