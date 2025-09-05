package mill.integration

import mill.testkit.{IntegrationTester, UtestIntegrationTestSuite}
import utest._

object CsProxyTests extends UtestIntegrationTestSuite {

  def isCI = System.getenv("CI") != null

  def proxyTest(
      wrongExtraEnv: (os.Path, Int) => Map[String, String],
      rightExtraEnv: (os.Path, Int) => Map[String, String],
      reset: IntegrationTester => Unit
  ): Unit = integrationTest { tester =>
    import tester._

    val proxyPort = TestAuthProxy.randomPort()

    val csCache = workspacePath / "tmp/cache"

    TestAuthProxy.withAuthProxy(workspacePath, port = proxyPort) {

      // We test the Mill launcher first

      System.err.println("First wrong check")

      // with the wrong proxy credentials, downloads must fail
      val wrongCredentialsRes = eval(
        "version",
        env = Map("COURSIER_CACHE" -> csCache.toString) ++ wrongExtraEnv(workspacePath, proxyPort),
        check = false
      )
      assert(wrongCredentialsRes.exitCode == 1)
      // assert(wrongCredentialsRes.err.contains("Error downloading io.get-coursier.jvm.indices"))
      assert(wrongCredentialsRes.err.contains("Unable to tunnel through proxy."))
      assert(wrongCredentialsRes.err.contains("407 Proxy Authentication Required"))
      pprint.err.log(wrongCredentialsRes)

      System.err.println("Reset")
      reset(tester)

      System.err.println("First right check")

      // but they should succeed with the right ones
      eval(
        "version",
        env = Map("COURSIER_CACHE" -> csCache.toString) ++ rightExtraEnv(workspacePath, proxyPort),
        check = true,
        stderr = if (isCI) os.Pipe else os.Inherit
      )

      System.err.println("Reset")
      reset(tester)

      // Now we test javalib / scalalib

      System.err.println("Second wrong check")

      // with the wrong proxy credentials, downloads must fail
      val wrongCredentialsCompileRes = eval(
        ("show", "compileClasspath"),
        env = Map("COURSIER_CACHE" -> csCache.toString) ++ wrongExtraEnv(workspacePath, proxyPort),
        check = false
      )
      assert(wrongCredentialsCompileRes.exitCode == 1)
      assert(wrongCredentialsCompileRes.err.contains("Unable to tunnel through proxy."))
      assert(wrongCredentialsCompileRes.err.contains("407 Proxy Authentication Required"))
      pprint.err.log(wrongCredentialsCompileRes)

      System.err.println("Reset")
      reset(tester)

      System.err.println("Second right check")

      // but they should succeed with the right ones
      eval(
        ("show", "compileClasspath"),
        env = Map("COURSIER_CACHE" -> csCache.toString) ++ rightExtraEnv(workspacePath, proxyPort),
        check = true,
        stderr = if (isCI) os.Pipe else os.Inherit
      )

      ()
    }
  }

  val tests: Tests = Tests {
    test("m2settings") {
      proxyTest(
        wrongExtraEnv = { (workspacePath, proxyPort) =>
          val nopeM2Dir = workspacePath / "tmp/m2-nope"
          os.write.over(
            nopeM2Dir / "settings.xml",
            TestAuthProxy.m2Settings(proxyPort, "wrong", "nope"),
            createFolders = true
          )
          Map("CS_MAVEN_HOME" -> nopeM2Dir.toString)
        },
        rightExtraEnv = { (workspacePath, proxyPort) =>
          val okM2Dir = workspacePath / "tmp/m2-ok"
          os.write.over(
            okM2Dir / "settings.xml",
            TestAuthProxy.m2Settings(proxyPort),
            createFolders = true
          )
          Map("CS_MAVEN_HOME" -> okM2Dir.toString)
        },
        reset = { tester =>
          import tester._
          // Need to shutdown the daemon to reset JDK internal HTTP stuff
          eval("shutdown", check = true, stderr = os.Inherit)
        }
      )
    }

    test("java props") {
      def jvmOpts(proxyPort: Int, user: String, password: String): Seq[String] =
        Seq(
          "-Dhttps.proxyProtocol=http",
          "-Dhttps.proxyHost=localhost",
          s"-Dhttps.proxyPort=$proxyPort",
          s"-Dhttps.proxyUser=$user",
          s"-Dhttps.proxyPassword=$password"
        )
      proxyTest(
        wrongExtraEnv = { (workspacePath, proxyPort) =>
          if (!os.exists(workspacePath / "build.mill.orig"))
            os.copy(workspacePath / "build.mill", workspacePath / "build.mill.orig")
          os.write.over(
            workspacePath / "build.mill",
            s"//| mill-jvm-opts: [${jvmOpts(proxyPort, "wrong", "nope").map("\"" + _ + "\"").mkString(", ")}]" +
              System.lineSeparator() +
              os.read(workspacePath / "build.mill.orig")
          )
          Map.empty
        },
        rightExtraEnv = { (workspacePath, proxyPort) =>
          if (!os.exists(workspacePath / "build.mill.orig"))
            os.copy(workspacePath / "build.mill", workspacePath / "build.mill.orig")
          os.write.over(
            workspacePath / "build.mill",
            s"//| mill-jvm-opts: [${jvmOpts(proxyPort, "jack", "insecure").map("\"" + _ + "\"").mkString(", ")}]" +
              System.lineSeparator() +
              os.read(workspacePath / "build.mill.orig")
          )
          Map.empty
        },
        reset = { tester =>
          import tester._
          // Need to shutdown the daemon to reset JDK internal HTTP stuff
          eval("shutdown", check = true, stderr = os.Inherit)
        }
      )
    }
  }
}
