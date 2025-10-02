package mill.bsp.worker

import java.util.concurrent.CompletableFuture

import scala.jdk.CollectionConverters.*

import ch.epfl.scala.bsp4j.{
  JavaBuildServer,
  JavacOptionsItem,
  JavacOptionsParams,
  JavacOptionsResult
}
import mill.api.daemon.internal.JavaModuleApi
import mill.bsp.worker.Utils.sanitizeUri
import mill.api.daemon.internal.ModuleRefApi

private trait MillJavaBuildServer extends JavaBuildServer { this: MillBuildServer =>

  override def buildTargetJavacOptions(javacOptionsParams: JavacOptionsParams)
      : CompletableFuture[JavacOptionsResult] =
    handlerTasks(
      targetIds = _ => javacOptionsParams.getTargets.asScala,
      tasks = {
        // We ignore all non-JavaModule
        case ref @ ModuleRefApi(_: JavaModuleApi, _) =>
          val ref0 = ref.asInstanceOf[ModuleRefApi[JavaModuleApi]]
          ref0
            .taskApi { m =>
              m.bspJavaModule().bspBuildTargetJavacOptions(
                sessionInfo.clientWantsSemanticDb,
                crossValues = Map.empty
              )
            }
            .unresolved(Map.empty)
      },
      requestDescription = "Getting javac options of {}",
      originId = ""
    ) {
      case (ev, _, id, _, f) =>
        val res = f(ev)
        new JavacOptionsItem(
          id,
          res.javacOptions.asJava,
          res.classpath.asJava,
          sanitizeUri(res.classesPath)
        )

    } {
      new JavacOptionsResult(_)
    }
}
