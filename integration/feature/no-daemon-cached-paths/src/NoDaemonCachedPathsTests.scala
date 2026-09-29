package mill.integration

import mill.testkit.UtestIntegrationTestSuite
import utest.*

object NoDaemonCachedPathsTests extends UtestIntegrationTestSuite {
  val tests: Tests = Tests {

    test("cachedAbsPathsOutliveTheRun") - integrationTest { tester =>
      // With --no-daemon, each run sees the workspace through forwarder symlinks under
      // out/mill-no-daemon/<run-id>/, which go away with the run. The absolute paths computed
      // by cached tasks shouldn't go through them, as they are reused by later runs.
      val res1 = tester.eval("check")
      assert(res1.isSuccess)
      assert(res1.out.contains("exists: true"))

      val res2 = tester.eval("check")
      assert(res2.isSuccess)
      assert(res2.out.contains("exists: true"))
    }
  }
}
