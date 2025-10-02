package mill.scalalib.bsp

import mill.api.{Cross, Discover, ModuleRef}
import mill.T
import mill.scalalib.{DepSyntax, JavaModule, ScalaModule}
import mill.testkit.UnitTester
import mill.testkit.TestRootModule
import utest.*
import mill.util.TokenReaders.*

object BspModuleTests extends TestSuite {
  val testScalaVersion = sys.props.getOrElse("TEST_SCALA_2_13_VERSION", ???)

  object MultiBase extends TestRootModule {
    object HelloBsp extends ScalaModule {
      def scalaVersion = testScalaVersion
      override def mvnDeps = Seq(mvn"org.slf4j:slf4j-api:1.7.34")
    }
    object HelloBsp2 extends ScalaModule {
      def scalaVersion = testScalaVersion
      override def moduleDeps = Seq(HelloBsp)
      override def mvnDeps = Seq(mvn"ch.qos.logback:logback-classic:1.1.10")
    }
    lazy val millDiscover = Discover[this.type]
  }

  object InterDeps extends TestRootModule {
    val maxCrossCount = 15
    val configs = 1.to(maxCrossCount)
    object Mod extends Cross[ModCross](configs)
    trait ModCross extends ScalaModule with Cross.Module[Int] {
      override def scalaVersion: T[String] = testScalaVersion
      // each depends on all others with lower index
      override def moduleDeps: Seq[JavaModule | ModuleRef[JavaModule]] =
        configs
          .filter(c => c < crossValue)
          .map(i => Mod(i))
    }
    lazy val millDiscover = Discover[this.type]
  }

  override def tests: Tests = Tests {
    test("nope") {}
  }
}
