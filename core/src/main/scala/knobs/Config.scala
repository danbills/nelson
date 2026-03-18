package knobs

import cats.effect.Sync
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import com.typesafe.config.{ConfigFactory, ConfigParseOptions, ConfigResolveOptions}

/** Minimal knobs stub.
 *  Replaces `io.verizon.knobs` (config library) which has no Scala 3 release.
 *  Backed by TypeSafe Config (HOCON) for actual config file parsing.
 *  The knobs config format is HOCON-compatible so files can be parsed directly.
 */
final class Config(val underlying: com.typesafe.config.Config) {

  /** Return a sub-config scoped to the given prefix (dotted path). */
  def subconfig(prefix: String): Config = {
    if (underlying.hasPath(prefix))
      new Config(underlying.getConfig(prefix))
    else
      new Config(ConfigFactory.empty())
  }

  /** Require a value; throws if not found or wrong type. */
  def require[A](key: String)(using ev: Knobs[A]): A =
    lookup[A](key).getOrElse(
      throw new NoSuchElementException(s"No such key: $key")
    )

  /** Look up an optional value. */
  def lookup[A](key: String)(using ev: Knobs[A]): Option[A] =
    if (underlying.hasPath(key)) ev.read(underlying, key)
    else None

  /** Merge two Configs. The right-hand side (other) takes precedence. */
  def ++(other: Config): Config =
    new Config(other.underlying.withFallback(underlying).resolve(ConfigResolveOptions.defaults().setAllowUnresolved(true)))

  override def toString: String = underlying.toString
}

object Config {
  val empty: Config = new Config(ConfigFactory.empty())
}

/** Type class for reading typed values from TypeSafe Config. */
trait Knobs[A] {
  def read(cfg: com.typesafe.config.Config, key: String): Option[A]
}

object Knobs {
  given Knobs[String] with {
    def read(cfg: com.typesafe.config.Config, key: String): Option[String] =
      Some(cfg.getString(key))
  }

  given Knobs[Int] with {
    def read(cfg: com.typesafe.config.Config, key: String): Option[Int] =
      Some(cfg.getInt(key))
  }

  given Knobs[Long] with {
    def read(cfg: com.typesafe.config.Config, key: String): Option[Long] =
      Some(cfg.getLong(key))
  }

  given Knobs[Boolean] with {
    def read(cfg: com.typesafe.config.Config, key: String): Option[Boolean] =
      Some(cfg.getBoolean(key))
  }

  given Knobs[Double] with {
    def read(cfg: com.typesafe.config.Config, key: String): Option[Double] =
      Some(cfg.getDouble(key))
  }

  given Knobs[FiniteDuration] with {
    def read(cfg: com.typesafe.config.Config, key: String): Option[FiniteDuration] = {
      try {
        val nanos = cfg.getDuration(key, java.util.concurrent.TimeUnit.NANOSECONDS)
        Some(nanos.nanoseconds)
      } catch {
        case _: Exception =>
          parseDuration(cfg.getString(key).trim)
      }
    }

    private def parseDuration(s: String): Option[FiniteDuration] = {
      val parts = s.split("\\s+", 2)
      if (parts.length == 2)
        parts(0).toLongOption.map(n => Duration(n, parts(1).trim.toLowerCase))
          .collect { case fd: FiniteDuration => fd }
      else None
    }
  }

  given Knobs[Duration] with {
    def read(cfg: com.typesafe.config.Config, key: String): Option[Duration] =
      try {
        val nanos = cfg.getDuration(key, java.util.concurrent.TimeUnit.NANOSECONDS)
        Some(nanos.nanoseconds: Duration)
      } catch {
        case _: Exception => None
      }
  }

  given [A](using inner: Knobs[A]): Knobs[Option[A]] with {
    def read(cfg: com.typesafe.config.Config, key: String): Option[Option[A]] =
      Some(inner.read(cfg, key))
  }

  given Knobs[List[String]] with {
    def read(cfg: com.typesafe.config.Config, key: String): Option[List[String]] =
      Some(cfg.getStringList(key).asScala.toList)
  }

  given Knobs[List[Int]] with {
    def read(cfg: com.typesafe.config.Config, key: String): Option[List[Int]] =
      Some(cfg.getIntList(key).asScala.map(_.intValue).toList)
  }
}

/** Represents a config resource location. */
sealed trait KnobsResource

/** Marks a resource as required (throws if not found). */
case class Required(resource: Resource) extends KnobsResource

/** Marks a resource as optional (silently ignored if not found). */
case class Optional(resource: Resource) extends KnobsResource

/** A resource file location. */
sealed trait Resource extends KnobsResource

/** Load a config file from the classpath. */
case class ClassPathResource(path: String) extends Resource

/** Load a config file from the filesystem. */
case class FileResource(file: java.io.File) extends Resource

/** Load config from system properties. */
case class SysPropsResource(spec: String) extends Resource

/** Companion object for loadImmutable — matches knobs API. */
object knobs {

  /** Load and merge a list of config resources into a single Config.
   *
   *  Files are loaded in order and merged; later entries take precedence.
   *  Config files use HOCON format (knobs .cfg files are HOCON-compatible).
   */
  def loadImmutable[F[_]](resources: List[KnobsResource])(using F: Sync[F]): F[Config] =
    F.delay {
      val configs = resources.flatMap {
        case Required(ClassPathResource(path)) =>
          val opts = ConfigParseOptions.defaults().setAllowMissing(false)
          val cfg = ConfigFactory.parseResourcesAnySyntax(path, opts)
          List(cfg)
        case Optional(ClassPathResource(path)) =>
          val opts = ConfigParseOptions.defaults().setAllowMissing(true)
          List(ConfigFactory.parseResourcesAnySyntax(path, opts))
        case Required(FileResource(file)) =>
          val opts = ConfigParseOptions.defaults().setAllowMissing(false)
          List(ConfigFactory.parseFile(file, opts))
        case Optional(FileResource(file)) =>
          List(ConfigFactory.parseFile(file))
        case Required(SysPropsResource(_)) =>
          List(ConfigFactory.systemProperties())
        case Optional(SysPropsResource(_)) =>
          List(ConfigFactory.systemProperties())
        case ClassPathResource(path) =>
          // treat bare resource as required
          val opts = ConfigParseOptions.defaults().setAllowMissing(false)
          List(ConfigFactory.parseResourcesAnySyntax(path, opts))
        case FileResource(file) =>
          List(ConfigFactory.parseFile(file))
        case _ =>
          Nil
      }
      // Merge: later entries override earlier entries (use withFallback in reverse)
      val merged = configs.foldLeft(ConfigFactory.empty())((acc, c) => c.withFallback(acc))
      new Config(merged.resolve(ConfigResolveOptions.defaults().setAllowUnresolved(true)))
    }
}
