package mill.bsp.worker

import ch.epfl.scala.bsp4j.{
  BuildClient,
  BuildTarget,
  BuildTargetCapabilities,
  BuildTargetIdentifier,
  OutputPathItem,
  OutputPathItemKind,
  StatusCode,
  TaskId
}
import mill.api.ExecResult.{Skipped, Success}
import mill.api.daemon.internal.ExecutionResultsApi

import scala.jdk.CollectionConverters.*
import scala.util.chaining.scalaUtilChainingOps
import mill.api.daemon.internal.ModuleRefApi
import mill.api.daemon.internal.bsp.{BspBuildTarget, BspModuleApi}
import java.net.URI
import java.net.URLEncoder
import java.nio.charset.StandardCharsets

private[mill] object Utils {

  def sanitizeUri(uri: String): String =
    if (uri.endsWith("/")) sanitizeUri(uri.substring(0, uri.length - 1)) else uri

  def sanitizeUri(uri: java.nio.file.Path): String = sanitizeUri(uri.toUri.toString)

  def encodeQuery(query: Seq[(String, String)]): String =
    if (query.isEmpty) ""
    else
      query
        .map {
          case (k, v) =>
            URLEncoder.encode(k, StandardCharsets.UTF_8) +
              "=" +
              URLEncoder.encode(v, StandardCharsets.UTF_8)
        }
        .mkString("?", "&", "")

  // define the function that spawns compilation reporter for each module based on the
  // module's hash code TODO: find something more reliable than the hash code
  def getBspLoggedReporterPool(
      originId: String,
      bspIdsByModule: Map[ModuleRefApi[BspModuleApi], BuildTargetIdentifier],
      client: BuildClient
  ): Int => Option[BspCompileProblemReporter] = { (moduleHashCode: Int) =>
    // FIXME Don't use moduleHashCode, but rather the render string of the module, including cross-values
    bspIdsByModule.find(_._1().hashCode == moduleHashCode).map {
      case (moduleRef, targetId) =>
        val buildTarget = moduleRef().bspBuildTarget.addCrossValues(moduleRef.crossValues)
        val taskId = new TaskId(moduleRef.hashCode.toString)
        new BspCompileProblemReporter(
          client,
          targetId,
          buildTarget.displayName.getOrElse(targetId.getUri),
          taskId,
          Option(originId)
        )
    }
  }

  // Get the execution status code given the results from Evaluator.evaluate
  def getStatusCode(resultsLists: Seq[ExecutionResultsApi]): StatusCode = {
    val statusCodes = resultsLists.flatMap(getStatusCodePerTask(_))
    if (statusCodes.contains(StatusCode.ERROR)) StatusCode.ERROR
    else if (statusCodes.contains(StatusCode.CANCELLED)) StatusCode.CANCELLED
    else StatusCode.OK
  }

  def makeBuildTarget(
      id: BuildTargetIdentifier,
      depsIds: Seq[BuildTargetIdentifier],
      bt: BspBuildTarget,
      data: Option[(String, Object)]
  ): BuildTarget = {
    val buildTarget = new BuildTarget(
      id,
      bt.tags.asJava,
      bt.languageIds.asJava,
      depsIds.asJava,
      new BuildTargetCapabilities().tap { it =>
        it.setCanCompile(bt.canCompile)
        it.setCanTest(bt.canTest)
        it.setCanRun(bt.canRun)
        it.setCanDebug(bt.canDebug)
      }
    )

    bt.displayName.foreach(buildTarget.setDisplayName)
    bt.baseDirectory.foreach(p => buildTarget.setBaseDirectory(sanitizeUri(p)))

    for ((dataKind, data) <- data) {
      buildTarget.setDataKind(dataKind)
      buildTarget.setData(data)
    }
    buildTarget
  }

  def outputPaths(
      buildTargetBaseDir: os.Path,
      topLevelProjectRoot: os.Path
  ): Seq[OutputPathItem] = {

    def outputPathItem(path: os.Path) =
      // Spec says, a directory must end with a forward slash
      new OutputPathItem(sanitizeUri(path.toNIO) + "/", OutputPathItemKind.DIRECTORY)

    if (topLevelProjectRoot.startsWith(buildTargetBaseDir))
      Seq(
        outputPathItem(topLevelProjectRoot / ".idea"),
        outputPathItem(topLevelProjectRoot / "out"),
        outputPathItem(topLevelProjectRoot / ".bsp"),
        outputPathItem(topLevelProjectRoot / ".bloop"),

        // All Eclipse JDT related project files (likely generated)
        outputPathItem(topLevelProjectRoot / ".project"),
        outputPathItem(topLevelProjectRoot / ".classpath"),
        outputPathItem(topLevelProjectRoot / ".settings")
      )
    else Nil
  }

  private def getStatusCodePerTask(results: ExecutionResultsApi): Seq[StatusCode] = {
    results.transitiveResultsApi.valuesIterator.map {
      case Success(_) => StatusCode.OK
      case Skipped => StatusCode.CANCELLED
      case _ => StatusCode.ERROR
    }.toSeq
  }

}
