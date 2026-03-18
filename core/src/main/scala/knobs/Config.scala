package knobs

import scala.reflect.ClassTag
import scala.concurrent.duration._

/** Minimal knobs stub.
 *  Replaces `io.verizon.knobs` (config library) which has no Scala 3 release.
 *  Backed by a flat Map[String, String] with prefix-based sub-configuration.
 *  Values are read via simple string conversion — real implementation would
 *  parse a knobs config file.
 */
final class Config(val env: Map[String, Any]) {

  /** Return a sub-config scoped to the given prefix. */
  def subconfig(prefix: String): Config = {
    val stripped = env.collect {
      case (k, v) if k.startsWith(prefix + ".") => k.drop(prefix.length + 1) -> v
    }
    new Config(stripped)
  }

  /** Require a value; throws if not found or wrong type. */
  def require[A](key: String)(implicit ev: Knobs[A]): A =
    lookup[A](key).getOrElse(
      throw new NoSuchElementException(s"knobs: required key '$key' not found in config")
    )

  /** Look up an optional value. */
  def lookup[A](key: String)(implicit ev: Knobs[A]): Option[A] =
    env.get(key).flatMap(ev.read)
}

object Config {
  val empty: Config = new Config(Map.empty)

  def apply(pairs: (String, Any)*): Config = new Config(pairs.toMap)
}

/** Type class for reading knobs values from their stored representation. */
trait Knobs[A] {
  def read(v: Any): Option[A]
}

object Knobs {
  given Knobs[String] with {
    def read(v: Any): Option[String] = v match {
      case s: String => Some(s)
      case other     => Some(other.toString)
    }
  }

  given Knobs[Int] with {
    def read(v: Any): Option[Int] = v match {
      case i: Int    => Some(i)
      case s: String => s.toIntOption
      case _         => None
    }
  }

  given Knobs[Long] with {
    def read(v: Any): Option[Long] = v match {
      case l: Long   => Some(l)
      case i: Int    => Some(i.toLong)
      case s: String => s.toLongOption
      case _         => None
    }
  }

  given Knobs[Boolean] with {
    def read(v: Any): Option[Boolean] = v match {
      case b: Boolean => Some(b)
      case s: String  => s.toBooleanOption
      case _          => None
    }
  }

  given Knobs[Double] with {
    def read(v: Any): Option[Double] = v match {
      case d: Double => Some(d)
      case i: Int    => Some(i.toDouble)
      case s: String => s.toDoubleOption
      case _         => None
    }
  }

  given Knobs[FiniteDuration] with {
    def read(v: Any): Option[FiniteDuration] = v match {
      case d: FiniteDuration => Some(d)
      case s: String =>
        // parse simple duration strings like "30 seconds", "5 minutes"
        val parts = s.trim.split("\\s+", 2)
        if (parts.length == 2)
          parts(0).toLongOption.map(n => Duration(n, parts(1).trim))
            .collect { case fd: FiniteDuration => fd }
        else None
      case _ => None
    }
  }

  given Knobs[Duration] with {
    def read(v: Any): Option[Duration] = v match {
      case d: Duration => Some(d)
      case s: String   =>
        val parts = s.trim.split("\\s+", 2)
        if (parts.length == 2)
          parts(0).toLongOption.map(n => Duration(n, parts(1).trim))
        else None
      case _ => None
    }
  }

  given [A](using inner: Knobs[A]): Knobs[Option[A]] with {
    def read(v: Any): Option[Option[A]] = Some(inner.read(v))
  }

  given [A](using inner: Knobs[A]): Knobs[List[A]] with {
    def read(v: Any): Option[List[A]] = v match {
      case xs: List[?] => Some(xs.flatMap(x => inner.read(x)))
      case _           => None
    }
  }
}
