package mill.api

import mill.api.*
import mill.api.daemon.internal.{ResolvedTaskApi, TaskApi, ExecutionResultsApi}
import mill.api.ResolvedTask

/**
 * The output of executing tasks via an [[Evaluator]]
 */
trait ExecutionResults extends ExecutionResultsApi {

  def goals: Seq[ResolvedTask[?]]

  /**
   * The values returned by the tasks specified by the user
   */
  def results: Seq[ExecResult[Val]]

  /**
   * The full mapping of all tasks transitively upstream of the specified
   * tasks, and their results
   */
  def transitiveResults: Map[ResolvedTask[?], ExecResult[Val]]
  private[mill] def transitiveResultsApi: Map[ResolvedTaskApi[?], ExecResult[Val]] =
    transitiveResults.map {
      case (task, res) =>
        (task, res)
    }
  private[mill] def transitiveTaskResultsApi(task: ResolvedTaskApi[?])
      : Seq[(ResolvedTaskApi[?], ExecResult[Val])] = {
    // FIXME Optimize that?
    transitiveResults
      .filter(_._1 == task)
      .toSeq
  }

  /**
   * The tasks that were executed without being read from cache
   */
  def uncached: Seq[ResolvedTask[?]]

  /**
   * The tasks and failures returned by failing tasks in [[transitiveResults]]
   */
  def transitiveFailing: Map[ResolvedTask[?], ExecResult.Failing[Val]] =
    transitiveResults.collect { case (k, v: ExecResult.Failing[Val]) => (k, v) }
  private[mill] def transitiveFailingApi: Map[ResolvedTaskApi[?], ExecResult.Failing[Val]] =
    transitiveFailing.map {
      case (task, res) =>
        (task, res)
    }

  /**
   * The values returned by successful tasks in [[results]]
   */
  def values: Seq[Val] = results.collect { case ExecResult.Success(v) => v }
}
