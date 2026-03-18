package journal

import org.slf4j.{Logger => Slf4jLogger, LoggerFactory}

/** Minimal journal.Logger stub backed by SLF4J.
 *  Replaces the `io.verizon.journal` library which has no Scala 3 release.
 */
final class Logger(underlying: Slf4jLogger) {
  def debug(msg: => String): Unit = if (underlying.isDebugEnabled) underlying.debug(msg)
  def debug(msg: => String, t: Throwable): Unit = if (underlying.isDebugEnabled) underlying.debug(msg, t)
  def info(msg: => String): Unit = if (underlying.isInfoEnabled) underlying.info(msg)
  def info(msg: => String, t: Throwable): Unit = if (underlying.isInfoEnabled) underlying.info(msg, t)
  def warn(msg: => String): Unit = if (underlying.isWarnEnabled) underlying.warn(msg)
  def warn(msg: => String, t: Throwable): Unit = if (underlying.isWarnEnabled) underlying.warn(msg, t)
  def error(msg: => String): Unit = if (underlying.isErrorEnabled) underlying.error(msg)
  def error(msg: => String, t: Throwable): Unit = if (underlying.isErrorEnabled) underlying.error(msg, t)
}

object Logger {
  def apply[A](implicit ct: reflect.ClassTag[A]): Logger =
    new Logger(LoggerFactory.getLogger(ct.runtimeClass))

  def apply(cls: Class[?]): Logger =
    new Logger(LoggerFactory.getLogger(cls))
}
