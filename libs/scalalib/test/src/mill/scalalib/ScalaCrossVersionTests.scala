package mill.scalalib

import mill.*
import mill.testkit.TestRootModule
import utest.*
import HelloWorldTests.*
import mill.api.Discover

object ScalaCrossVersionTests extends TestSuite {

  object CrossModuleDeps extends TestRootModule {
    def scalaVersion = Task.CrossValue(Seq(scala212Version, scala32Version))
    object stable extends ScalaModule {
      def scalaVersion = CrossModuleDeps.scalaVersion
    }

    object cuttingEdge extends ScalaModule {
      def moduleDeps = Seq(stable)
      def scalaVersion =
        CrossModuleDeps.scalaVersion.withValues(Seq(scala213Version, scala33Version))
    }

    lazy val millDiscover = Discover[this.type]
  }

  def tests: Tests = Tests {

    test("scala-33-depend-on-scala-32-works") {
      // FIXME Actually compute the plan of a 3.3 task from CrossModuleDeps.cuttingEdge
      // and check that it depends on a 3.2 task from CrossModuleDeps.stable
      CrossModuleDeps.cuttingEdge.moduleDeps
    }
    test("scala-213-depend-on-scala-212-fails") {
      // FIXME Actually compute the plan of a 2.13 task from CrossModuleDeps.cuttingEdge
      // and check that this fails for a lack of cross-value match

      // val message = assertThrows[Exception](
      //   CrossModuleDeps.cuttingEdge(scala213Version).moduleDeps
      // ).getMessage
      // assert(
      //   message == s"Unable to find compatible cross version between ${scala213Version} and 2.12.6,3.2.0"
      // )
    }
  }
}
