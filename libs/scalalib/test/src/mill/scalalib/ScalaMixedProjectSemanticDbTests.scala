package mill.scalalib

import mill.api.Discover
import mill.testkit.{TestRootModule, UnitTester}
import mill.util.TokenReaders.*
import utest.*
import mill.api.Evaluator

object ScalaMixedProjectSemanticDbTests extends TestSuite {

  object SemanticWorld extends TestRootModule {
    object core extends HelloWorldTests.SemanticModule

    lazy val millDiscover = Discover[this.type]
  }

  val resourcePath = os.Path(sys.env("MILL_TEST_RESOURCE_DIR")) / "hello-world-mixed"

  def tests: Tests = Tests {

    test("semanticDbData") {
      def semanticDbFiles: Set[os.SubPath] = Set(
        os.sub / "META-INF/semanticdb/core/src/Foo.scala.semanticdb",
        os.sub / "META-INF/semanticdb/core/src/JFoo.java.semanticdb",
        os.sub / "foo/JFoo.class",
        os.sub / "foo/Foo.class",
        os.sub / "foo/Foo$.class"
      )

      test("fromScratch") - UnitTester(SemanticWorld, sourceRoot = resourcePath, isBsp = true).scoped { eval =>
        val task = Evaluator.isBsp.withValue(true) {
          SemanticWorld.core.compile
        }

        {
          println("first - expected full compile")
          val Right(result) = eval.apply(task): @unchecked

          val dataPath = eval.outPath / "core/compile.dest/classes"
          val outputFiles = os.walk(result.value.classes.path)
            .filter(os.isFile)
            .map(_.subRelativeTo(result.value.classes.path))

          val expectedSemFiles = semanticDbFiles
          assert(
            result.value.classes.path == dataPath,
            outputFiles.nonEmpty,
            outputFiles.toSet == expectedSemFiles,
            result.evalCount > 0,
            os.exists(dataPath / os.up / "zinc")
          )
        }
        {
          println("second - expected no compile")
          // don't recompile if nothing changed
          val Right(result2) = eval.apply(task): @unchecked
          assert(result2.evalCount == 0)
        }
      }
    }
  }
}
