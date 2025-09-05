package mill.integration

import java.net.ServerSocket

import scala.concurrent.duration._

object TestAuthProxy {

  private def imageId = "alpine:3.21.2"

  def randomPort(): Int = {
    val s = new ServerSocket(0)
    try s.getLocalPort()
    finally s.close()
  }

  def withAuthProxy[T](workingDir: os.Path, port: Int)(f: => T): T = {
    var containerId: String = ""
    try {
      containerId =
        os.proc(
          "docker",
          "run",
          "-d",
          "--rm",
          "-v",
          s"$workingDir:/data",
          "-p",
          s"$port:80",
          imageId,
          "/bin/sh",
          "/data/run.sh"
        )
          .call().out.trim()
      var serverListening = false
      var delay = 1.second
      var count = 0
      while (!serverListening && count < 5) {
        val lines = os.proc("docker", "logs", containerId).call().out.lines()
        serverListening = lines.contains("Starting server")
        if (!serverListening) {
          System.err.println(s"Waiting for auth proxy to listen on localhost:$port")
          Thread.sleep(delay.toMillis)
          delay = 2 * delay
          count += 1
        }
      }
      Thread.sleep(5000L) // give the server a few seconds to start just in case
      f
    } finally
      if (containerId.nonEmpty)
        os.proc("docker", "rm", "-f", containerId).call(stdin = os.Inherit, stdout = os.Inherit)
  }

  def m2Settings(port: Int, user: String, password: String): String =
    s"""<settings>
       |<proxies>
       |   <proxy>
       |      <id>test-proxy</id>
       |      <protocol>http</protocol>
       |      <host>localhost</host>
       |      <port>$port</port>
       |      <username>$user</username>
       |      <password>$password</password>
       |    </proxy>
       |  </proxies>
       |</settings>
       |""".stripMargin

  def m2Settings(port: Int): String =
    m2Settings(port, "jack", "insecure")

}
