package mill.api

import mill.api.*
import mill.api.daemon.internal.{ResolvedTaskApi, ExecutionResultsApi}
import mill.api.ResolvedTask
import scala.collection.immutable.ListMap
import mill.api.daemon.internal.UnresolvedTaskApi

/**
 * The output of executing tasks via an [[Evaluator]]
 */
trait ExecutionResults extends ExecutionResultsApi {

  def goals: ListMap[UnresolvedTask[?], Seq[ResolvedTask[?]]]
  final def goalsApi: ListMap[UnresolvedTaskApi[?], Seq[ResolvedTaskApi[?]]] =
    goals.asInstanceOf[ListMap[UnresolvedTaskApi[?], Seq[ResolvedTaskApi[?]]]]

  /**
   * The values returned by the tasks specified by the user
   */
  def results: Seq[Seq[ExecResult[Val]]]

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

  def transitivePrefixes: Map[ResolvedTask[?], Seq[String]] = Map()
  private[mill] override def transitivePrefixesApi: Map[ResolvedTaskApi[?], Seq[String]] =
    transitivePrefixes.asInstanceOf[Map[ResolvedTaskApi[?], Seq[String]]]

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

  // FIXME Look for calls to values.flatten, and check if anything suspicious happens
  /**
   * The values returned by successful tasks in [[results]]
   */
  def values: Seq[Seq[Val]] = results.map(_.collect { case ExecResult.Success(v) => v })
}
