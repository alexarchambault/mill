package mill.exec

import mill.api.{Discover, Module}
import mill.Task
import mill.testkit.{TestRootModule, UnitTester}
import mill.testkit.UnitTester.Result
import utest.*

object CrossValueTests extends TestSuite {

  object simple extends TestRootModule {
    def firstParam = Task.CrossValue(Seq("a", "b"))

    object myCross extends Module {
      def param1 = Task { "Param Value: " + firstParam() }
    }

    def secondParam = Task.CrossValue(Seq(("a", 1), ("b", 2)))

    object myCrossExtended extends Module {
      def param2 = Task { "Param Value: " + secondParam() }
    }

    // object myCrossExtendedAgain
    //     extends Cross[MyCrossModuleExtendedAgain](("a", 1, true), ("b", 2, false))
    // trait MyCrossModuleExtendedAgain extends MyCrossModuleExtended
    //     with Cross.Module3[String, Int, Boolean] {
    //   def param3 = Task { "Param Value: " + crossValue3 }
    // }

    lazy val millDiscover = Discover[this.type]
  }

  val tests = Tests {
    test("simple") {
      UnitTester(simple, null).scoped { check =>
        val Right(Result("Param Value: a", _)) =
          check(simple.myCross.param1, Map("firstParam" -> "a")): @unchecked

        val Right(Result("a", _)) =
          check(simple.firstParam, Map("firstParam" -> "a")): @unchecked

        val Right(Result("b", _)) =
          check(simple.firstParam, Map("firstParam" -> "b")): @unchecked

        val Right(Result("Param Value: b", _)) =
          check(simple.myCross.param1, Map("firstParam" -> "b")): @unchecked
      }
    }
  }
}
