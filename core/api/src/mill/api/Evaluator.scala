package mill.api

import mill.api.daemon.internal.{CompileProblemReporter, TestReporter}
import mill.api.*
import mill.api.daemon.Watchable
import mill.api.BuildCtx
import mill.api.ResolvedTask
import mill.api.daemon.internal.{EvaluatorApi, UnresolvedTaskApi}
import mill.api.internal.{Resolved, RootModule0}

import scala.util.DynamicVariable
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

/**
 * An API that allows you to resolve, plan, and execute Mill tasks.
 *
 * [[Evaluator]] can be taken as a parameter to [[Task.Command]]s marked as `exclusive = true`,
 * providing those commands with the ability to inspect the build and dynamically decide what
 * to evaluate. Many builtin commands like `show`, `plan`, `path`, etc. are implemented in
 * this way
 */
trait Evaluator extends AutoCloseable with EvaluatorApi {
  private[mill] def allowPositionalCommandArgs: Boolean
  private[mill] def selectiveExecution: Boolean
  private[mill] def workspace: os.Path
  private[mill] def baseLogger: Logger
  private[mill] def outPath: os.Path
  private[mill] def outPathJava = outPath.toNIO
  private[mill] def codeSignatures: Map[String, Int]
  private[mill] def rootModule: RootModule0
  private[mill] def workerCache: mutable.Map[String, (Int, Val)]
  private[mill] def env: Map[String, String]
  private[mill] def effectiveThreadCount: Int
  private[mill] def offline: Boolean

  def withBaseLogger(newBaseLogger: Logger): Evaluator

  def resolveSegments(
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      allowPositionalCommandArgs: Boolean = false,
      resolveToModuleTasks: Boolean = false
  ): mill.api.Result[List[Segments.WithCrossValues]]

  private[mill] def resolveRaw(
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      allowPositionalCommandArgs: Boolean = false,
      resolveToModuleTasks: Boolean = false
  ): mill.api.Result[List[Resolved]] = {
    // These are used in the overrides.
    val _ = scriptArgs
    val _ = selectMode
    val _ = allowPositionalCommandArgs
    val _ = resolveToModuleTasks

    mill.api.Result.Success(Nil)
  }

  def resolveTasks(
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      allowPositionalCommandArgs: Boolean = false,
      resolveToModuleTasks: Boolean = false
  ): mill.api.Result[List[UnresolvedTask.Named[?]]]
  def resolveModulesOrTasks(
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      allowPositionalCommandArgs: Boolean = false,
      resolveToModuleTasks: Boolean = false
  ): mill.api.Result[List[Either[Module, UnresolvedTask.Named[?]]]]

  def plan(tasks: Seq[UnresolvedTask[?]]): mill.api.Result[Plan]

  private[mill] override def resolvedTasks(task: UnresolvedTaskApi[?])
      : mill.api.Result[Seq[ResolvedTask[?]]] = {
    val task0 = task match {
      case t: UnresolvedTask[?] => t
      case _ => ???
    }

    plan(Seq(task0)).map(_.goals)
  }

  def groupAroundImportantTasks[T](
      topoSortedTasks: mill.api.TopoSorted[ResolvedTask[?]],
      plan: Plan
  )(
      important: PartialFunction[
        ResolvedTask[?],
        T
      ]
  ): MultiBiMap[T, ResolvedTask[?]]

  /**
   * Collects all transitive dependencies (tasks) of the given tasks,
   * including the given tasks.
   */
  def transitiveTasks(sourceNodes: Seq[ResolvedTask[_]])(
      inputsFor: ResolvedTask[_] => Seq[ResolvedTask[_]]
  ): IndexedSeq[ResolvedTask[_]]

  /**
   * Takes the given tasks, finds all the tasks they transitively depend
   * on, and sort them topologically. Fails if there are dependency cycles
   */
  def topoSorted[T: ClassTag](transitiveTasks: IndexedSeq[T], inputs: T => Seq[T]): TopoSorted[T]

  private[mill] def executeApi[T](
      tasks: Seq[UnresolvedTaskApi[T]]
  ): mill.api.Result[Evaluator.Result[T]] =
    execute[T](
      tasks.map {
        case task: UnresolvedTask[T] => task
        case _ => ???
      }
    )

  def execute[T](
      tasks: Seq[UnresolvedTask[T]],
      reporter: Int => Option[CompileProblemReporter] = _ => Option.empty[CompileProblemReporter],
      testReporter: TestReporter = TestReporter.DummyTestReporter,
      logger: Logger = baseLogger,
      serialCommandExec: Boolean = false,
      selectiveExecution: Boolean = false
  ): mill.api.Result[Evaluator.Result[T]]

  def evaluate(
      scriptArgs: Seq[String],
      selectMode: SelectMode = SelectMode.Separated,
      reporter: Int => Option[CompileProblemReporter] = _ => None,
      selectiveExecution: Boolean = false
  ): mill.api.Result[Evaluator.Result[Any]]

  private[mill] def executeApi[T](
      tasks: Seq[UnresolvedTaskApi[T]],
      reporter: Int => Option[CompileProblemReporter] = _ => Option.empty[CompileProblemReporter],
      testReporter: TestReporter = TestReporter.DummyTestReporter,
      logger: Logger = null,
      serialCommandExec: Boolean = false,
      selectiveExecution: Boolean = false
  ): mill.api.Result[EvaluatorApi.Result[T]] = {
    BuildCtx.withFilesystemCheckerDisabled {
      execute(
        tasks.map {
          case task: UnresolvedTask[T] => task
          case _ => ???
        },
        reporter,
        testReporter,
        logger,
        serialCommandExec,
        selectiveExecution
      )
    }
  }

  /**
   * APIs related to selective execution
   */
  def selective: SelectiveExecution
}
object Evaluator {
  // This needs to be a ThreadLocal because we need to pass it into the body of
  // the EvaluatorTokenReader#read call, which does not accept additional parameters.
  // Until we migrate our CLI parsing off of Scopt (so we can pass the BaseModule
  // in directly) we are forced to pass it in via a ThreadLocal
  private val currentEvaluator0 = new DynamicVariable[Evaluator](null)
  private[mill] def withCurrentEvaluator[T](ev: Evaluator)(t: => T) = {
    // Make sure we only put a `EvaluatorProxy` in the `DynamicVariable` rather than a
    // raw `Evaluator`, and `.close()` it after we're done to `null` out the contents.
    // This is so that if the `InheritableThreadLocal` gets captured by long-lived spawned
    // thread pools, we do not end up leaking the contents.
    scala.util.Using.resource(new EvaluatorProxy(() => ev)) { ev2 =>
      currentEvaluator0.withValue(ev2) {
        t
      }
    }
  }

  private[mill] def currentEvaluator = currentEvaluator0.value

  /**
   * @param watchable the list of [[Watchable]]s that were generated during this evaluation,
   *                  useful if you want to know what to watch in case you need to re-run it.
   * @param values A sequence of values returned by evaluation.
   * @param selectedTasks The tasks that actually were selected to be run during this evaluation
   * @param executionResults Detailed information on the results of executing each task
   */
  final case class Result[T](
      watchable: Seq[Watchable],
      values: mill.api.Result[Seq[T]],
      selectedTasks: Seq[ResolvedTask[?]],
      executionResults: ExecutionResults
  ) extends EvaluatorApi.Result[T]

  private[mill] val defaultEnv: Map[String, String] = System.getenv().asScala.toMap
}
