package mill.api.daemon.internal

import mill.api.daemon.*
import scala.collection.mutable
import scala.collection.immutable.ListMap

trait EvaluatorApi extends AutoCloseable {
  def evaluate(
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      reporter: Int => Option[CompileProblemReporter] = _ => None,
      selectiveExecution: Boolean = false
  ): Result[EvaluatorApi.Result[Any]]

  private[mill] def executeApi[T](
      tasks: Seq[UnresolvedTaskApi[T]],
      reporter: Int => Option[CompileProblemReporter] = _ => Option.empty[CompileProblemReporter],
      testReporter: TestReporter = TestReporter.DummyTestReporter,
      logger: Logger = null,
      serialCommandExec: Boolean = false,
      selectiveExecution: Boolean = false
  ): mill.api.daemon.Result[EvaluatorApi.Result[T]]

  private[mill] def workerCache: mutable.Map[String, (Int, Val)]

  private[mill] def executeApi[T](
      tasks: Seq[UnresolvedTaskApi[T]]
  ): mill.api.daemon.Result[EvaluatorApi.Result[T]]
  private[mill] def baseLogger: Logger
  private[mill] def rootModule: BaseModuleApi
  private[mill] def outPathJava: java.nio.file.Path

  private[mill] def resolvedTasks(task: UnresolvedTaskApi[?])
      : mill.api.daemon.Result[Seq[ResolvedTaskApi[?]]]
}
object EvaluatorApi {
  trait Result[T] {
    def watchable: Seq[Watchable]
    def values: mill.api.daemon.Result[Seq[T]]

    def selectedTasks: Seq[ResolvedTaskApi[?]]
    def executionResults: ExecutionResultsApi
  }
}

trait ExecutionResultsApi {
  def goalsApi: ListMap[UnresolvedTaskApi[?], Seq[ResolvedTaskApi[?]]]
  def results: Seq[Seq[ExecResult[Val]]]
  private[mill] def transitiveResultsApi: Map[ResolvedTaskApi[?], ExecResult[Val]]
  private[mill] def transitiveTaskResultsApi(task: ResolvedTaskApi[?])
      : Seq[(ResolvedTaskApi[?], ExecResult[Val])]

  private[mill] def transitiveFailingApi: Map[ResolvedTaskApi[?], ExecResult.Failing[Val]]
  private[mill] def transitivePrefixesApi: Map[ResolvedTaskApi[?], Seq[String]] = Map()
  def uncached: Seq[ResolvedTaskApi[?]]

  def values: Seq[Seq[Val]]

  lazy val resultsMap: ListMap[UnresolvedTaskApi[?], Seq[(ResolvedTaskApi[?], ExecResult[Val])]] =
    goalsApi
      .zip(results)
      .map {
        case ((inputTask, resolvedTasks), results0) =>
          (inputTask, resolvedTasks.zip(results0))
      }
      .to(ListMap)
}
object ExecutionResultsApi {
  private[mill] def formatFailing(evaluated: ExecutionResultsApi): String = {
    (for ((k, fs) <- evaluated.transitiveFailingApi)
      yield {
        val fss = fs match {
          case ExecResult.Failure(t) => t
          case ex: ExecResult.Exception => ex.toString
        }
        val keyPrefix = Logger.formatPrefix(evaluated.transitivePrefixesApi.getOrElse(k, Nil))
        s"$keyPrefix${k.displayName} $fss"
      }).mkString("\n")
  }

}
