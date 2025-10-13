package mill.scalalib

import mill.*
import mill.api.Discover
import mill.testkit.UnitTester
import mill.testkit.TestRootModule
import utest.*

object ScalaVersionsRangesTests extends TestSuite {

  def scala212 = "2.12.13"
  def scala213 = "2.13.5"
  def scala3 = "3.3.3"

  object ScalaVersionsRanges extends TestRootModule {
    def scalaVersion = Task.CrossValue(Seq(scala212, scala213, scala3))
    object core extends ScalaModule {
      def scalaVersion = ScalaVersionsRanges.scalaVersion
      object test extends ScalaTests with TestModule.Utest {
        override def utestVersion = "0.8.5"
      }
    }

    lazy val millDiscover = Discover[this.type]
  }
  val resourcePath =
    os.Path(sys.env("MILL_TEST_RESOURCE_DIR")) / "scala-versions-ranges"

  val tests = Tests {
    test("main with Scala 2.12- and 2.13+ specific code") - UnitTester(
      ScalaVersionsRanges,
      resourcePath
    ).scoped { eval =>
      val Right(_) =
        eval(ScalaVersionsRanges.core.run().unresolved(Map("scalaVersion" -> scala212))): @unchecked
      val Right(_) =
        eval(ScalaVersionsRanges.core.run().unresolved(Map("scalaVersion" -> scala213))): @unchecked
      val Right(_) =
        eval(ScalaVersionsRanges.core.run().unresolved(Map("scalaVersion" -> scala3))): @unchecked
    }
    test("test with Scala 2.12- and 2.13+ specific code") - UnitTester(
      ScalaVersionsRanges,
      resourcePath
    ).scoped { eval =>
      val Right(_) = eval(
        ScalaVersionsRanges.core.test.testForked().unresolved(Map("scalaVersion" -> scala212))
      ): @unchecked
      val Right(_) = eval(
        ScalaVersionsRanges.core.test.testForked().unresolved(Map("scalaVersion" -> scala213))
      ): @unchecked
      val Right(_) = eval(
        ScalaVersionsRanges.core.test.testForked().unresolved(Map("scalaVersion" -> scala3))
      ): @unchecked
    }
  }
}
