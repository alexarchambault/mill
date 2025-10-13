package mill.api

import mill.api.*
import mill.api.daemon.*
import mill.api.daemon.internal.*
import mill.api.internal.*

import scala.reflect.ClassTag

final class EvaluatorProxy(var delegate0: () => Evaluator) extends Evaluator {
  private def delegate = delegate0()
  override def allowPositionalCommandArgs = delegate.allowPositionalCommandArgs
  override def selectiveExecution = delegate.selectiveExecution
  override def workspace = delegate.workspace
  override def baseLogger = delegate.baseLogger
  override def outPath = delegate.outPath
  override def codeSignatures = delegate.codeSignatures
  override def rootModule = delegate.rootModule
  override def workerCache = delegate.workerCache
  override def env = delegate.env
  override def effectiveThreadCount = delegate.effectiveThreadCount
  override def offline: Boolean = delegate.offline

  def withBaseLogger(newBaseLogger: Logger): Evaluator = delegate.withBaseLogger(newBaseLogger)

  def resolveSegments(
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      allowPositionalCommandArgs: Boolean,
      resolveToModuleTasks: Boolean
  ): mill.api.Result[List[Segments.WithCrossValues]] = {
    delegate.resolveSegments(
      scriptArgs,
      selectMode,
      allowPositionalCommandArgs,
      resolveToModuleTasks
    )
  }

  override private[mill] def resolveRaw(
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      allowPositionalCommandArgs: Boolean,
      resolveToModuleTasks: Boolean
  ): mill.api.Result[List[Resolved]] = {
    delegate.resolveRaw(
      scriptArgs,
      selectMode,
      allowPositionalCommandArgs,
      resolveToModuleTasks
    )
  }

  def resolveTasks(
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      allowPositionalCommandArgs: Boolean = false,
      resolveToModuleTasks: Boolean = false
  ): mill.api.Result[List[UnresolvedTask.Named[?]]] = {
    delegate.resolveTasks(scriptArgs, selectMode, allowPositionalCommandArgs, resolveToModuleTasks)
  }
  def resolveModulesOrTasks(
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      allowPositionalCommandArgs: Boolean = false,
      resolveToModuleTasks: Boolean = false
  ): mill.api.Result[List[Either[Module, UnresolvedTask.Named[?]]]] = {
    delegate.resolveModulesOrTasks(
      scriptArgs,
      selectMode,
      allowPositionalCommandArgs,
      resolveToModuleTasks
    )
  }
  def plan(tasks: Seq[UnresolvedTask[?]]): mill.api.Result[Plan] = delegate.plan(tasks)

  def groupAroundImportantTasks[T](
      topoSortedTasks: mill.api.TopoSorted[ResolvedTask[?]],
      plan: Plan
  )(
      important: PartialFunction[
        ResolvedTask[?],
        T
      ]
  ): MultiBiMap[T, ResolvedTask[?]] =
    delegate.groupAroundImportantTasks(topoSortedTasks, plan)(important)

  def transitiveTasks(sourceNodes: Seq[ResolvedTask[_]])(
      inputsFor: ResolvedTask[_] => Seq[ResolvedTask[_]]
  ): IndexedSeq[ResolvedTask[_]] =
    delegate.transitiveTasks(sourceNodes)(inputsFor)

  def topoSorted[T: ClassTag](transitiveTasks: IndexedSeq[T], inputs: T => Seq[T]): TopoSorted[T] =
    delegate.topoSorted(transitiveTasks, inputs)

  def execute[T](
      tasks: Seq[UnresolvedTask[T]],
      reporter: Int => Option[CompileProblemReporter] = _ => Option.empty[CompileProblemReporter],
      testReporter: TestReporter = TestReporter.DummyTestReporter,
      logger: Logger = baseLogger,
      serialCommandExec: Boolean = false,
      selectiveExecution: Boolean = false
  ): mill.api.Result[Evaluator.Result[T]] = {
    delegate.execute(
      tasks,
      reporter,
      testReporter,
      logger,
      serialCommandExec,
      selectiveExecution
    )
  }

  def evaluate(
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      reporter: Int => Option[CompileProblemReporter] = _ => None,
      selectiveExecution: Boolean = false
  ): mill.api.Result[Evaluator.Result[Any]] = {
    delegate.evaluate(scriptArgs, selectMode, reporter, selectiveExecution)
  }
  def close = delegate0 = null

  def selective = delegate.selective
}
