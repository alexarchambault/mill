package mill.integration

import mill.testkit.UtestIntegrationTestSuite
import utest._

object CsProxyTests extends UtestIntegrationTestSuite {

  def isCI = System.getenv("CI") != null

  val tests: Tests = Tests {
    test("proxy") - integrationTest { tester =>
      import tester._

      val proxyPort = TestAuthProxy.randomPort()

      val okM2Dir = workspacePath / "tmp/m2-ok"
      os.write(okM2Dir / "settings.xml", TestAuthProxy.m2Settings(proxyPort), createFolders = true)
      val nopeM2Dir = workspacePath / "tmp/m2-nope"
      os.write(
        nopeM2Dir / "settings.xml",
        TestAuthProxy.m2Settings(proxyPort, "wrong", "nope"),
        createFolders = true
      )

      val csCache = workspacePath / "tmp/cache"

      TestAuthProxy.withAuthProxy(workspacePath, port = proxyPort) {

        // We test the Mill launcher first

        // with the wrong proxy credentials, downloads must fail
        val wrongCredentialsRes = eval(
          "version",
          env = Map(
            "COURSIER_CACHE" -> csCache.toString,
            "CS_MAVEN_HOME" -> nopeM2Dir.toString
          ),
          check = false
        )
        assert(wrongCredentialsRes.exitCode == 1)
        assert(wrongCredentialsRes.err.contains("Unable to tunnel through proxy."))
        assert(wrongCredentialsRes.err.contains("407 Proxy Authentication Required"))

        // but they should succeed with the right ones
        eval(
          "version",
          env = Map(
            "COURSIER_CACHE" -> csCache.toString,
            "CS_MAVEN_HOME" -> okM2Dir.toString
          ),
          check = true,
          stderr = if (isCI) os.Pipe else os.Inherit
        )

        // Need to shutdown the daemon to reset JDK internal HTTP stuff
        eval("shutdown", check = true, stderr = os.Inherit)

        // Now we test javalib / scalalib

        // with the wrong proxy credentials, downloads must fail
        val wrongCredentialsCompileRes = eval(
          ("show", "compileClasspath"),
          env = Map(
            "COURSIER_CACHE" -> csCache.toString,
            "CS_MAVEN_HOME" -> nopeM2Dir.toString
          ),
          check = false
        )
        assert(wrongCredentialsCompileRes.exitCode == 1)
        assert(wrongCredentialsCompileRes.err.contains("Unable to tunnel through proxy."))
        assert(wrongCredentialsCompileRes.err.contains("407 Proxy Authentication Required"))

        // Need to shutdown the daemon to reset JDK internal HTTP stuff
        eval("shutdown", check = true, stderr = os.Inherit)

        // but they should succeed with the right ones
        eval(
          ("show", "compileClasspath"),
          env = Map(
            "COURSIER_CACHE" -> csCache.toString,
            "CS_MAVEN_HOME" -> okM2Dir.toString
          ),
          check = true,
          stderr = if (isCI) os.Pipe else os.Inherit
        )

        ()
      }
    }
  }
}
