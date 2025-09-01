package mill.util

import java.util.concurrent.LinkedBlockingQueue
import coursier.core.Repository
import mill.api.{PathRef, Discover, Evaluator, ExternalModule, MultiBiMap, SelectMode}
import mill.*
import mill.api.{Result}
import org.jgrapht.graph.{DefaultEdge, SimpleDirectedGraph}
import guru.nidi.graphviz.attribute.Rank.RankDir
import guru.nidi.graphviz.attribute.{Rank, Shape, Style}
import mill.api.BuildCtx
import mill.api.UnresolvedTask
import mill.api.ResolvedNamedTask
import mill.api.ResolvedTask

object VisualizeModule extends ExternalModule {
  def repositories: Seq[Repository] =
    coursier.Resolve.defaultRepositories

  lazy val millDiscover = Discover[this.type]

  private type VizWorker = (
      LinkedBlockingQueue[(
          MultiBiMap[ResolvedNamedTask[Any], ResolvedTask[?]],
          mill.api.Plan,
          os.Path
      )],
      LinkedBlockingQueue[Result[scala.Seq[PathRef]]]
  )

  private[mill] def visualize0(
      evaluator: Evaluator,
      tasks: Seq[String],
      ctx: mill.api.TaskCtx,
      vizWorker: VizWorker,
      planTasks: Option[List[Task.Named[?]]] = None
  ): Result[Seq[PathRef]] = {
    def callVisualizeModule(
        transitiveTasks: List[UnresolvedTask[Any]]
    ): Result[Seq[PathRef]] = {
      val (in, out) = vizWorker
      val plan = evaluator.plan(transitiveTasks)
      val transitive = evaluator.transitiveTasks0(plan.goals)(plan.inputs(_))
      val topoSorted = evaluator.topoSorted0(transitive, plan.inputs)
      val sortedGroups = evaluator.groupAroundImportantTasks0(topoSorted, plan) {
        case t if t.asNamed.map(_.task).exists(n => transitiveTasks.exists(_.task == n)) =>
          t.asNamed.get
      }
      in.put((sortedGroups, plan, ctx.dest))
      val res = out.take()
      res.map { v =>
        println(upickle.write(v.map(_.path.toString()), indent = 2))
        v
      }
    }

    evaluator.resolveTasks(tasks, SelectMode.Multi).flatMap {
      rs =>
        // FIXME Get those via the Seq[String]
        val crossValues = Map.empty[String, String]
        val rs0 = rs.map(_.unresolved(crossValues))
        planTasks match {
          case Some(allRs) =>
            val allRs0 = allRs.map(_.unresolved(crossValues))
            callVisualizeModule(allRs0)
          case None => callVisualizeModule(rs0)
        }
    }
  }

  @deprecated("Use toolsClasspath instead", "0.13.0-M1")
  def classpath = toolsClasspath

  def toolsClasspath: T[Seq[PathRef]] = Task {
    mill.util.Jvm.resolveDependencies(
      repositories = repositories,
      deps = Seq(
        coursier.Dependency(
          coursier.Module(
            coursier.Organization("com.lihaoyi"),
            coursier.ModuleName("mill-libs-graphviz_3")
          ),
          coursier.VersionConstraint(BuildInfo.millVersion)
        )
      ),
      force = Nil
    ).map(_.map(_.withRevalidateOnce))
  }

  /**
   * The J2V8-based Graphviz library has a limitation that it can only ever
   * be called from a single thread. Since Mill forks off a new thread every
   * time you execute something, we need to keep around a worker thread that
   * everyone can use to call into Graphviz, which the Mill execution threads
   * can communicate via in/out queues.
   */
  private[mill] def worker: Worker[(
      LinkedBlockingQueue[(
          MultiBiMap[ResolvedNamedTask[Any], ResolvedTask[?]],
          mill.api.Plan,
          os.Path
      )],
      LinkedBlockingQueue[Result[Seq[PathRef]]]
  )] = mill.api.Task.Worker {
    val in =
      new LinkedBlockingQueue[(
          MultiBiMap[ResolvedNamedTask[Any], ResolvedTask[?]],
          mill.api.Plan,
          os.Path
      )]()
    val out = new LinkedBlockingQueue[Result[Seq[PathRef]]]()
    val visualizeThread = new java.lang.Thread(() =>
      while (true) {
        val res = Result.Success {
          val (sortedGroups, plan, dest) = in.take()

          val goalSet = plan.goals.toSet
          import guru.nidi.graphviz.model.Factory._
          val edgesIterator =
            for ((k, vs) <- sortedGroups.items())
              yield (
                k,
                for {
                  v <- vs
                  dest <- plan.inputs(v).flatMap(_.asNamed)
                  if goalSet.contains(dest.asTask)
                } yield dest
              )

          val edges = edgesIterator.map { case (k, v) => (k, v.toArray.distinct) }.toArray

          val indexToTask = edges.flatMap { case (k, vs) => Iterator(k) ++ vs }.distinct
          val taskToIndex = indexToTask.zipWithIndex.toMap

          val jgraph = new SimpleDirectedGraph[Int, DefaultEdge](classOf[DefaultEdge])

          for (i <- indexToTask.indices) jgraph.addVertex(i)
          for ((src, dests) <- edges; dest <- dests) {
            jgraph.addEdge(taskToIndex(src), taskToIndex(dest))
          }

          org.jgrapht.alg.TransitiveReduction.INSTANCE.reduce(jgraph)
          val nodes = indexToTask.map(t =>
            node(plan.sortedGroups.lookupValue(t.asTask).displayName)
              .`with` {
                if (goalSet.contains(t.asTask)) Style.SOLID
                else Style.DASHED
              }
              .`with`(Shape.BOX)
          )

          var g = graph("example1").directed
          for (i <- indexToTask.indices) {
            for {
              e <- edges(i)._2
              j = taskToIndex(e)
              if jgraph.containsEdge(i, j)
            } {
              g = g.`with`(nodes(j).link(nodes(i)))
            }
          }

          g = g.graphAttr().`with`(Rank.dir(RankDir.LEFT_TO_RIGHT))

          mill.util.Jvm.callProcess(
            mainClass = "mill.graphviz.GraphvizTools",
            classPath = toolsClasspath().map(_.path).toVector,
            mainArgs = Seq(s"${os.temp(g.toString)};$dest;txt,dot,json,png,svg"),
            stdin = os.Inherit,
            stdout = os.Inherit
          )

          BuildCtx.withFilesystemCheckerDisabled {
            os.list(dest).sorted.map(PathRef(_))
          }
        }
        out.put(res)
      }
    )
    visualizeThread.setDaemon(true)
    visualizeThread.start()
    (in, out)
  }
}
