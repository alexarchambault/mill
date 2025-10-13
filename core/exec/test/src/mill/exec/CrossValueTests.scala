package mill.exec

import mill.api.{Discover, Module}
import mill.api.daemon.ExecResult
import mill.Task
import mill.testkit.{TestRootModule, UnitTester}
import mill.testkit.UnitTester.Result
import utest.*

object CrossValueTests extends TestSuite {

  object simple extends TestRootModule {
    def firstParam: Task.CrossValue[String] = Task.CrossValue(Seq("a", "b"))

    def source = Task.Source {
      os.sub / "thing" / s"foo-${firstParam()}"
    }

    object myCross extends Module {
      def param1 = Task { "Param Value: " + firstParam() }
    }

    object myCrossWithRestrictions extends Module {
      def restrictedFirstParam = firstParam.withValues(Seq("b"))
      def param1 = Task { "Restricted param Value: " + restrictedFirstParam() }
    }

    lazy val millDiscover = Discover[this.type]
  }

  val tests = Tests {
    test("simple") {
      simpleTest()
    }
  }

  def simpleTest(): Unit =
    UnitTester(simple, null).scoped { check =>
      val Right(Result(Seq("a", "b"), _)) =
        check(simple.firstParam.unresolved(Map.empty)): @unchecked

      val Right(Result(Seq("a"), _)) =
        check(simple.firstParam.unresolved(Map("firstParam" -> "a"))): @unchecked

      val Right(Result(Seq("b"), _)) =
        check(simple.firstParam.unresolved(Map("firstParam" -> "b"))): @unchecked

      val Right(Result(Seq("Param Value: a"), _)) =
        check(simple.myCross.param1.unresolved(Map("firstParam" -> "a"))): @unchecked

      val Right(Result(Seq("Param Value: b"), _)) =
        check(simple.myCross.param1.unresolved(Map("firstParam" -> "b"))): @unchecked

      val Left(ExecResult.Failure("firstParam doesn't accept value 'c' (allowed values: a, b)")) =
        check(simple.myCross.param1.unresolved(Map("firstParam" -> "c"))): @unchecked

      val Right(Result(Seq("Param Value: a"), _)) =
        check("__.param1,firstParam=a"): @unchecked

      val Right(Result(Seq("Param Value: b", "Restricted param Value: b"), _)) =
        check("__.param1,firstParam=b"): @unchecked

      val Left(ExecResult.Failure(
        "firstParam doesn't accept value 'c' (allowed values: a, b), firstParam doesn't accept value 'c' (allowed values: b)"
      )) =
        check("__.param1,firstParam=c"): @unchecked

      val Right(Result(sourceRes, _)) =
        check(simple.source.unresolved(Map.empty)): @unchecked
      val sourceRes0 = sourceRes.map(_.path.relativeTo(simple.moduleDir))
      val expectedSourceRes = Seq(
        os.sub / "thing/foo-a",
        os.sub / "thing/foo-b"
      )
      assert(sourceRes0 == expectedSourceRes)
      val Right(Result(Seq(pathRef), _)) =
        check(simple.source.unresolved(Map("firstParam" -> "a"))): @unchecked
      val sourcePath = pathRef.path.relativeTo(simple.moduleDir)
      assert(sourcePath == os.rel / "thing/foo-a")
    }
}
