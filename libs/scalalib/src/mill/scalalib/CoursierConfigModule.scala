package mill.scalalib

import coursier.CoursierEnv
import coursier.cache.{CacheEnv, CachePolicy}
import coursier.core.Repository
import coursier.credentials.Credentials
import coursier.params.Mirror
import coursier.util.{EnvEntry, EnvValues}
import mill.define.*
import mill.{T, Task}

import scala.concurrent.duration.Duration

object CoursierConfigModule extends ExternalModule {

  private val entries = Seq(
    CacheEnv.cache,
    CacheEnv.credentials,
    CacheEnv.ttl,
    CacheEnv.cachePolicy,
    CacheEnv.archiveCache,
    CoursierEnv.mirrors,
    CoursierEnv.mirrorsExtra,
    CoursierEnv.repositories,
    CoursierEnv.scalaCliConfig,
    CoursierEnv.configDir
  )

  private val envVars = entries.map(_.envName).toSet
  private val propNames = entries.map(_.propName).toSet

  extension (entry: EnvEntry)
    private def readFrom(env: Map[String, String], props: Map[String, String]): EnvValues =
      EnvValues(env.get(entry.envName), props.get(entry.propName))

  def coursierEnv: T[(Map[String, String], Map[String, String])] = Task.Input {
    val env = Task.env.filterKeys(envVars).toMap
    val props =
      propNames.iterator.flatMap(name => sys.props.get(name).iterator.map(name -> _)).toMap
    (env, props)
  }

  def defaultRepositories: Task[Seq[Repository]] = Task.Anon {
    val (env, props) = coursierEnv()
    CoursierEnv.defaultRepositories(
      CoursierEnv.repositories.readFrom(env, props),
      CoursierEnv.scalaCliConfig.readFrom(env, props)
    )
  }

  def defaultConfFiles: Task[Seq[PathRef]] = Task.Anon {
    val (env, props) = coursierEnv()
    CacheEnv.defaultConfFiles(CacheEnv.scalaCliConfig.readFrom(env, props))
      .map(os.Path(_, Task.workspace))
      .map(PathRef(_))
  }

  def defaultMirrors: Task[Seq[Mirror]] = Task.Anon {
    val (env, props) = coursierEnv()
    CoursierEnv.defaultMirrors(
      CoursierEnv.mirrors.readFrom(env, props),
      CoursierEnv.mirrorsExtra.readFrom(env, props),
      CoursierEnv.scalaCliConfig.readFrom(env, props),
      CoursierEnv.configDir.readFrom(env, props)
    )
  }

  def defaultCacheLocation: Task[String] = Task.Anon {
    val (env, props) = coursierEnv()
    val path: java.nio.file.Path = CacheEnv.defaultCacheLocation(
      CacheEnv.cache.readFrom(env, props)
    )
    path.toString
  }

  def defaultArchiveCacheLocation: Task[String] = Task.Anon {
    val (env, props) = coursierEnv()
    val path: java.nio.file.Path = CacheEnv.defaultArchiveCacheLocation(
      CacheEnv.archiveCache.readFrom(env, props)
    )
    path.toString
  }

  def defaultCredentials: Task[Seq[Credentials]] = Task.Anon {
    val (env, props) = coursierEnv()
    CacheEnv.defaultCredentials(
      CacheEnv.credentials.readFrom(env, props),
      CacheEnv.scalaCliConfig.readFrom(env, props),
      CacheEnv.configDir.readFrom(env, props)
    )
  }

  def defaultTtl: Task[Option[Duration]] = Task.Anon {
    val (env, props) = coursierEnv()
    CacheEnv.defaultTtl(
      CacheEnv.ttl.readFrom(env, props)
    )
  }

  def defaultCachePolicies: Task[Seq[CachePolicy]] = Task.Anon {
    val (env, props) = coursierEnv()
    CacheEnv.defaultCachePolicies(
      CacheEnv.cachePolicy.readFrom(env, props)
    )
  }

  def coursierConfig: Task[mill.util.Jvm.CoursierConfig] = Task.Anon {
    mill.util.Jvm.CoursierConfig(
      // defaultRepositories(),
      defaultConfFiles().map(_.path),
      defaultMirrors(),
      defaultCacheLocation(),
      defaultArchiveCacheLocation(),
      defaultCredentials(),
      defaultTtl(),
      defaultCachePolicies()
    )
  }

  override protected def millDiscover: Discover = Discover[this.type]
}
