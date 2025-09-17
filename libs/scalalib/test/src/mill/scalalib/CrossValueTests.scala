package mill.scalalib

import mill.api.Discover
import mill.util.TokenReaders.*
import mill.testkit.UnitTester
import mill.testkit.UnitTester.Result
import mill.testkit.TestRootModule
import utest.*
import mill.api.Cross
import mill.api.Task
import mill.api.ref

object CrossValueTests extends TestSuite {

  object modules extends TestRootModule {

    val scalaVersions = Seq("2.13.16", "3.3.4")

    // main cross value is defined here
    def theScalaVersion = Task.CrossValue(scalaVersions)

    // using SbtModule here that takes into account scalaVersion in its sources tasks

    // a module that relies on the cross value
    object crossValueModule extends SbtModule {
      def scalaVersion = theScalaVersion
    }

    // another module depending on the cross value and the other module using it
    object crossValueApp extends SbtModule {
      def scalaVersion = theScalaVersion
      def moduleDeps = Seq(
        crossValueModule
      )
    }

    // a legacy cross module, that can still depend on modules relying on the cross value
    trait LegacyCrossApp extends Cross.Module[String] with SbtModule {
      def scalaVersion = crossValue
      def moduleDeps = Seq(
        crossValueModule.ref.addCrossValues(Seq("theScalaVersion" -> crossValue))
      )
    }
    object legacyCrossApp extends Cross[LegacyCrossApp](scalaVersions)

    lazy val millDiscover = Discover[this.type]
  }

  def tests = Tests {
    test("cross-module") {
      UnitTester(modules, null).scoped { eval =>
        // eval the cross task itself - this should list its possible values
        val Right(Result(res, _)) = eval.apply(modules.theScalaVersion, Map()): @unchecked
        assert(res == Seq("2.13.16", "3.3.4"))

        val Right(Result(sourcesRes, _)) = eval(modules.crossValueModule.sources, Map()): @unchecked
        val sourcesRes0 = sourcesRes.map(_.map(_.path.relativeTo(modules.moduleDir)))

        val expectedSourceRes = Seq(
          Seq(
            os.sub / "crossValueModule/src/main/scala",
            os.sub / "crossValueModule/src/main/java",
            os.sub / "crossValueModule/src/main/scala-2.13.16",
            os.sub / "crossValueModule/src/main/scala-2.13",
            os.sub / "crossValueModule/src/main/scala-2"
          ),
          Seq(
            os.sub / "crossValueModule/src/main/scala",
            os.sub / "crossValueModule/src/main/java",
            os.sub / "crossValueModule/src/main/scala-3.3.4",
            os.sub / "crossValueModule/src/main/scala-3.3",
            os.sub / "crossValueModule/src/main/scala-3"
          )
        )

        assert(sourcesRes0 == expectedSourceRes)

        val Right(Result(compileCpRes, _)) =
          eval(modules.crossValueApp.compileClasspath, Map()): @unchecked
        val compileCpRes0 = compileCpRes.map { res =>
          res
            .map(_.path)
            .filter(_.startsWith(modules.moduleDir))
            .map(_.relativeTo(modules.moduleDir).asSubPath)
        }

        val expectedCompileCpRes = Seq(
          Seq(
            os.sub / "crossValueModule/compile-resources",
            os.sub / "out/crossValueModule/theScalaVersion/2.13.16/compile.dest/classes",
            os.sub / "crossValueApp/compile-resources"
          ),
          Seq(
            os.sub / "crossValueModule/compile-resources",
            os.sub / "out/crossValueModule/theScalaVersion/3.3.4/compile.dest/classes",
            os.sub / "crossValueApp/compile-resources"
          )
        )

        assert(compileCpRes0 == expectedCompileCpRes)

        val Right(Result(legacy213CrossAppRes, _)) =
          eval(modules.legacyCrossApp("2.13.16").compileClasspath, Map()): @unchecked
        val Right(Result(legacy3CrossAppRes, _)) =
          eval(modules.legacyCrossApp("3.3.4").compileClasspath, Map()): @unchecked
        val legacy213CrossAppRes0 = legacy213CrossAppRes.map { res =>
          res
            .map(_.path)
            .filter(_.startsWith(modules.moduleDir))
            .map(_.relativeTo(modules.moduleDir).asSubPath)
        }
        val legacy3CrossAppRes0 = legacy3CrossAppRes.map { res =>
          res
            .map(_.path)
            .filter(_.startsWith(modules.moduleDir))
            .map(_.relativeTo(modules.moduleDir).asSubPath)
        }

        val expectedLegacy213CompileCpRes = Seq(
          os.sub / "crossValueModule/compile-resources",
          os.sub / "out/crossValueModule/theScalaVersion/2.13.16/compile.dest/classes",
          os.sub / "legacyCrossApp/compile-resources"
        )
        val expectedLegacy3CompileCpRes = Seq(
          os.sub / "crossValueModule/compile-resources",
          os.sub / "out/crossValueModule/theScalaVersion/3.3.4/compile.dest/classes",
          os.sub / "legacyCrossApp/compile-resources"
        )

        assert(legacy213CrossAppRes0 == Seq(expectedLegacy213CompileCpRes))
        assert(legacy3CrossAppRes0 == Seq(expectedLegacy3CompileCpRes))

        val Right(Result(crossValueAppRunCpRes, _)) =
          eval(modules.crossValueApp.runClasspath, Map()): @unchecked
        val crossValueAppRunCpRes0 = crossValueAppRunCpRes.map { res =>
          res
            .map(_.path)
            .filter(_.startsWith(modules.moduleDir))
            .map(_.relativeTo(modules.moduleDir).asSubPath)
        }

        val expectedCrossValueAppRunCpRes0 = Seq(
          Seq(
            os.sub / "crossValueModule/compile-resources",
            os.sub / "crossValueModule/src/main/resources",
            os.sub / "out/crossValueModule/theScalaVersion/2.13.16/compile.dest/classes",
            os.sub / "crossValueApp/compile-resources",
            os.sub / "crossValueApp/src/main/resources",
            os.sub / "out/crossValueApp/theScalaVersion/2.13.16/compile.dest/classes"
          ),
          Seq(
            os.sub / "crossValueModule/compile-resources",
            os.sub / "crossValueModule/src/main/resources",
            os.sub / "out/crossValueModule/theScalaVersion/3.3.4/compile.dest/classes",
            os.sub / "crossValueApp/compile-resources",
            os.sub / "crossValueApp/src/main/resources",
            os.sub / "out/crossValueApp/theScalaVersion/3.3.4/compile.dest/classes"
          )
        )

        assert(crossValueAppRunCpRes0 == expectedCrossValueAppRunCpRes0)

        val Right(Result(legacy213CrossAppRunCpRes, _)) =
          eval(modules.legacyCrossApp("2.13.16").runClasspath, Map()): @unchecked
        val legacy213CrossAppRunCpRes0 = legacy213CrossAppRunCpRes.map { res =>
          res
            .map(_.path)
            .filter(_.startsWith(modules.moduleDir))
            .map(_.relativeTo(modules.moduleDir).asSubPath)
        }
        val Right(Result(legacy3CrossAppRunCpRes, _)) =
          eval(modules.legacyCrossApp("3.3.4").runClasspath, Map()): @unchecked
        val legacy3CrossAppRunCpRes0 = legacy3CrossAppRunCpRes.map { res =>
          res
            .map(_.path)
            .filter(_.startsWith(modules.moduleDir))
            .map(_.relativeTo(modules.moduleDir).asSubPath)
        }

        val expectedLegacy213CrossAppRunCpRes0 = Seq(
          os.sub / "crossValueModule/compile-resources",
          os.sub / "crossValueModule/src/main/resources",
          os.sub / "out/crossValueModule/theScalaVersion/2.13.16/compile.dest/classes",
          os.sub / "legacyCrossApp/compile-resources",
          os.sub / "legacyCrossApp/src/main/resources",
          os.sub / "out/legacyCrossApp/2.13.16/theScalaVersion/2.13.16/compile.dest/classes"
        )
        val expectedLegacy3CrossAppRunCpRes0 = Seq(
          os.sub / "crossValueModule/compile-resources",
          os.sub / "crossValueModule/src/main/resources",
          os.sub / "out/crossValueModule/theScalaVersion/3.3.4/compile.dest/classes",
          os.sub / "legacyCrossApp/compile-resources",
          os.sub / "legacyCrossApp/src/main/resources",
          os.sub / "out/legacyCrossApp/3.3.4/theScalaVersion/3.3.4/compile.dest/classes"
        )

        assert(legacy213CrossAppRunCpRes0 == Seq(expectedLegacy213CrossAppRunCpRes0))
        assert(legacy3CrossAppRunCpRes0 == Seq(expectedLegacy3CrossAppRunCpRes0))

        ()
      }
    }
  }
}
