package mill.exec

import mill.api.{Discover, Module}
import mill.api.daemon.ExecResult
import mill.Task
import mill.testkit.{TestRootModule, UnitTester}
import mill.testkit.UnitTester.Result
import utest.*

object CrossValueTests extends TestSuite {

  object simple extends TestRootModule {
    def firstParam: Task.CrossValue = Task.CrossValue(Seq("a", "b"))

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
      UnitTester(simple, null).scoped { check =>
        val Right(Result(Seq("a", "b"), _)) =
          check(simple.firstParam, Map()): @unchecked

        val Right(Result(Seq("a"), _)) =
          check(simple.firstParam, Map("firstParam" -> "a")): @unchecked

        val Right(Result(Seq("b"), _)) =
          check(simple.firstParam, Map("firstParam" -> "b")): @unchecked

        val Right(Result(Seq("Param Value: a"), _)) =
          check(simple.myCross.param1, Map("firstParam" -> "a")): @unchecked

        val Right(Result(Seq("Param Value: b"), _)) =
          check(simple.myCross.param1, Map("firstParam" -> "b")): @unchecked

        val Left(ExecResult.Failure("firstParam doesn't accept value 'c' (allowed values: a, b)")) =
          check(simple.myCross.param1, Map("firstParam" -> "c")): @unchecked

        val Right(Result(Seq("Param Value: a"), _)) =
          check(Seq("__.param1"), Map("firstParam" -> "a")): @unchecked

        val Right(Result(Seq("Param Value: b", "Restricted param Value: b"), _)) =
          check(Seq("__.param1"), Map("firstParam" -> "b")): @unchecked

        val Left(ExecResult.Failure(
          "firstParam doesn't accept value 'c' (allowed values: a, b), firstParam doesn't accept value 'c' (allowed values: b)"
        )) =
          check(Seq("__.param1"), Map("firstParam" -> "c")): @unchecked
      }
    }
  }
}
