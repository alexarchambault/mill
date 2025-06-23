package mill.define

import mill.api.*
import mill.api.internal.{AppliedTaskApi, ExecutionResultsApi, TaskApi}
import mill.define.Plan0.AppliedTask

trait ExecutionResults extends ExecutionResultsApi {

  /**
   * The values returned by the tasks specified by the user
   */
  def results: Seq[ExecResult[Val]]

  /**
   * The full mapping of all tasks transitively upstream of the specified
   * tasks, and their results
   */
  def transitiveResults: Map[AppliedTask[?], ExecResult[Val]]
  private[mill] def transitiveResultsApi: Map[AppliedTaskApi[?], ExecResult[Val]] =
    transitiveResults.map {
      case (task, res) =>
        (task, res)
    }
  private[mill] def transitiveTaskResultsApi(task: TaskApi[?])
      : Seq[(AppliedTaskApi[?], ExecResult[Val])] = {
    // FIXME Optimize that?
    transitiveResults
      .filter(_._1.task == task)
      .toSeq
  }

  /**
   * The tasks that were executed without being read from cache
   */
  def uncached: Seq[AppliedTask[?]]

  /**
   * The tasks and failures returned by failing tasks in [[transitiveResults]]
   */
  def transitiveFailing: Map[AppliedTask[?], ExecResult.Failing[Val]] =
    transitiveResults.collect { case (k, v: ExecResult.Failing[Val]) => (k, v) }
  private[mill] def transitiveFailingApi: Map[AppliedTaskApi[?], ExecResult.Failing[Val]] =
    transitiveFailing.map {
      case (task, res) =>
        (task, res)
    }

  /**
   * The values returned by successful tasks in [[results]]
   */
  def values: Seq[Val] = results.collect { case ExecResult.Success(v) => v }
}
