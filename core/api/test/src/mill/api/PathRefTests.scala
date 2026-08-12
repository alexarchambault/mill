package mill.api

import utest.*

import java.nio.file.Files
import java.nio.file.attribute.PosixFilePermissions
import scala.util.Properties

object PathRefTests extends TestSuite {
  val tests: Tests = Tests {
    test("sig") {
      def check(quick: Boolean) = withTmpDir { tmpDir =>
        val file = tmpDir / "foo.txt"
        os.write.over(file, "hello")
        val sig1 = PathRef(file, quick).sig
        val sig1b = PathRef(file, quick).sig
        assert(sig1 == sig1b)
        os.write.over(file, "hello world")
        val sig2 = PathRef(file, quick).sig
        assert(sig1 != sig2)
      }
      test("qref") - check(quick = true)
      test("ref") - check(quick = false)
    }

    test("same-sig-other-file") {
      def check(quick: Boolean) = withTmpDir { tmpDir =>
        val file = tmpDir / "foo.txt"
        os.write.over(file, "hello")
        val sig1 = PathRef(file, quick).sig
        val file2 = tmpDir / "bar.txt"
        os.copy(file, file2)
        val sig1b = PathRef(file2, quick).sig
        assert(sig1 == sig1b)
      }
//      test("qref") - check(quick = true)
      test("ref") - check(quick = false)
    }

    test("sub-millisecond-mtime") {
      // A quick sig is computed in one JVM and re-validated in another, which may report a
      // different sub-millisecond precision for the very same mtime (JDK 11 truncates Linux
      // mtimes to microseconds, later JDKs report nanoseconds). Anything below the millisecond
      // must therefore be ignored.
      withTmpDir { tmpDir =>
        val file = tmpDir / "foo.txt"
        os.write.over(file, "hello")

        def setMtime(nanos: Long): Unit = {
          val _ = Files.setLastModifiedTime(
            file.wrapped,
            java.nio.file.attribute.FileTime.from(nanos, java.util.concurrent.TimeUnit.NANOSECONDS)
          )
        }

        val millis = 1786529073356L
        setMtime(millis * 1000000L)
        val sig = PathRef(file, quick = true).sig

        // same millisecond, different micro- / nanoseconds
        setMtime(millis * 1000000L + 21128L)
        assert(PathRef(file, quick = true).sig == sig)
        setMtime(millis * 1000000L + 999999L)
        assert(PathRef(file, quick = true).sig == sig)

        // the millisecond itself is still taken into account
        setMtime((millis + 1) * 1000000L)
        assert(PathRef(file, quick = true).sig != sig)
      }
    }

    test("perms") {
      def check(quick: Boolean) =
        if (isPosixFs()) withTmpDir { tmpDir =>
          val file = tmpDir / "foo.txt"
          val content = "hello"
          os.write.over(file, content)
          Files.setPosixFilePermissions(file.wrapped, PosixFilePermissions.fromString("rw-rw----"))
          val rwSig = PathRef(file, quick).sig
          val rwSigb = PathRef(file, quick).sig
          assert(rwSig == rwSigb)

          Files.setPosixFilePermissions(file.wrapped, PosixFilePermissions.fromString("rwxrw----"))
          val rwxSig = PathRef(file, quick).sig

          assert(rwSig != rwxSig)
        }
        else "Test Skipped on non-POSIX host"

      test("qref") - check(quick = true)
      test("ref") - check(quick = false)
    }

    test("symlinks") {
      def check(quick: Boolean) = withTmpDir { tmpDir =>
        // invalid symlink
        os.symlink(tmpDir / "nolink", tmpDir / "nonexistant")

        // symlink to empty dir
        os.symlink(tmpDir / "emptylink", tmpDir / "empty")
        os.makeDir(tmpDir / "empty")

        // recursive symlinks
        os.symlink(tmpDir / "rlink1", tmpDir / "rlink2")
        os.symlink(tmpDir / "rlink2", tmpDir / "rlink1")

        val sig1 = PathRef(tmpDir, quick).sig
        val sig2 = PathRef(tmpDir, quick).sig
        assert(sig1 == sig2)
      }
      test("qref") - check(quick = true)
      test("ref") - check(quick = false)
    }

    test("json") {
      def check(quick: Boolean) = withTmpDir { tmpDir =>
        val file = tmpDir / "foo.txt"
        os.write(file, "hello")
        val pr = PathRef(file, quick)
        val prFile = pr.path.toString().replace("\\", "\\\\")
        val json = upickle.write(pr)
        if (quick) {
          assert(json.startsWith(""""qref:v0:"""))
          assert(json.endsWith(s""":${prFile}""""))
        } else {
          val hash = if (Properties.isWin) "86df6a6a" else "4c7ef487"
          val expected = s""""ref:v0:${hash}:${prFile}""""
          assert(json == expected)
        }
        val pr1 = upickle.read[PathRef](json)
        assert(pr == pr1)
      }

      test("qref") - check(quick = true)
      test("ref") - check(quick = false)
    }
  }

  private def withTmpDir[T](body: os.Path => T): T = {
    val tmpDir = os.Path(Files.createTempDirectory(""))
    val res = body(tmpDir)
    os.remove.all(tmpDir)
    res
  }

  private def isPosixFs(): Boolean = {
    java.nio.file.FileSystems.getDefault.supportedFileAttributeViews().contains("posix")
  }
}
