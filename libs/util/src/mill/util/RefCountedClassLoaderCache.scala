package mill.util

import mill.api.PathRef

import java.net.URLClassLoader
import scala.collection.mutable.LinkedHashMap

/**
 * Caches classloaders that can be shared between different workers, keeping
 * a reference count of each classloader and only closing it after no more
 * references exist
 */
class RefCountedClassLoaderCache(
    sharedLoader: ClassLoader = null,
    sharedPrefixes: Seq[String] = Nil,
    parent: ClassLoader = null,
    sharedClass: Option[Class[?]] = None,
    extraRelease: ClassLoader => Unit = _ => ()
) {
  // bin-compat shim
  def this(
      sharedLoader: ClassLoader,
      sharedPrefixes: Seq[String],
      parent: ClassLoader
  ) =
    this(
      sharedLoader,
      sharedPrefixes,
      parent,
      None
    )

  private val cache = LinkedHashMap.empty[Long, (URLClassLoader, Int)]

  def release(combinedCompilerJars: Seq[PathRef]) = synchronized {
    val compilersSig = combinedCompilerJars.hashCode()
    cache.updateWith(compilersSig) {
      case Some((cl, 1)) =>
        // We try to find the timer created by scala.tools.nsc.classpath.FileBasedCache
        // and cancel it, so that it shuts down its thread.
        extraRelease(cl)
        cl.close()
        None
      case Some((cl, n)) if n > 1 => Some((cl, n - 1))
      case v => sys.error("Unknown: " + v) // No other cases; n should never be zero or negative
    }

  }
  def get(
      combinedCompilerJars: Seq[PathRef]
  )(implicit e: sourcecode.Enclosing): URLClassLoader = synchronized {
    val compilersSig = combinedCompilerJars.hashCode()
    cache.get(compilersSig) match {
      case Some((cl, i)) =>
        cache(compilersSig) = (cl, i + 1)
        cl
      case _ =>
        // the Scala compiler must load the `xsbti.*` classes from the same loader as `JvmWorkerImpl`

        // When started with layered class loaders, likely by CoursierClient,
        // rely on that to get a clean base class loader to load the worker.
        // Else, likely when run from integration tests in local mode, fallback
        // on the shared class loader hack.
        val cl = sharedClass match {
          case Some(sharedClass0) =>
            mill.util.Jvm.createClassLoader(
              combinedCompilerJars.map(_.path),
              parent = sharedClass0.getClassLoader
            )(e)
          case None =>
            mill.util.Jvm.createClassLoader(
              combinedCompilerJars.map(_.path),
              parent = parent,
              sharedLoader = sharedLoader,
              sharedPrefixes = sharedPrefixes
            )(e)
        }
        cache.update(compilersSig, (cl, 1))
        cl
    }
  }

  def close() = {
    cache.values.map { case (cl, hash) => cl.close() }
    cache.clear()
  }

}
