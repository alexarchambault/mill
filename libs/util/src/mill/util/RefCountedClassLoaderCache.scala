package mill.util

import mill.api.PathRef

import java.net.URLClassLoader

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
    extraRelease0: ClassLoader => Unit = _ => ()
) extends AutoCloseable {
  // bin-compat stub
  def this(
      sharedLoader: ClassLoader,
      sharedPrefixes: Seq[String],
      parent: ClassLoader
  ) =
    this(
      sharedLoader,
      sharedPrefixes,
      parent,
      None,
      _ => ()
    )

  private val cache = RefCountedCache[Seq[PathRef], Long, sourcecode.Enclosing, URLClassLoader](
    convertKey = _.hashCode,
    setup = (combinedCompilerJars, _, enclosing) => {
      // the Scala compiler must load the `xsbti.*` classes from the same loader as `JvmWorkerImpl`

      // When started with layered class loaders, likely by CoursierClient,
      // rely on that to get a clean base class loader to load the worker.
      // Else, likely when run from integration tests in local mode, fallback
      // on the shared class loader hack.
      sharedClass match {
        case Some(sharedClass0) =>
          mill.util.Jvm.createClassLoader(
            combinedCompilerJars.map(_.path),
            parent = sharedClass0.getClassLoader
          )(using enclosing)
        case None =>
          mill.util.Jvm.createClassLoader(
            combinedCompilerJars.map(_.path),
            parent = parent,
            sharedLoader = sharedLoader,
            sharedPrefixes = sharedPrefixes
          )(using enclosing)
      }
    },
    closeValue = cl => {
      extraRelease(cl)
      cl.close()
    }
  )

  def extraRelease(cl: ClassLoader): Unit =
    extraRelease0(cl)

  def release(combinedCompilerJars: Seq[PathRef]): Option[(URLClassLoader, Int)] =
    cache.release(combinedCompilerJars).map { case RefCountedCache.Entry(value, refCount) =>
      (value, refCount)
    }

  def get(combinedCompilerJars: Seq[PathRef])(using e: sourcecode.Enclosing): URLClassLoader =
    cache.get(combinedCompilerJars, e)

  override def close(): Unit =
    cache.close()
}
