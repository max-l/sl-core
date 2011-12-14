package net.strong_links.core

import org.slf4j.{ LoggerFactory, Logger => SLF4JLogger }

object Logging {

  type GenericLogger = {
    def isErrorEnabled(): Boolean
    def isDebugEnabled(): Boolean
    def isInfoEnabled(): Boolean
    def isWarnEnabled(): Boolean
    def warn(s: String): Unit
    def error(s: String): Unit
    def info(s: String): Unit
    def debug(s: String): Unit
  }

  private val overrider = new ThreadLocalStack[GenericLogger]

  def using[R](logger: GenericLogger)(code: => R): R =
    overrider.using(logger)(code)
}

trait Logging {

  private lazy val slf4j = LoggerFactory.getLogger(this.getClass)

  private def actualLogger = {
    val l = Logging.overrider.unsafeGet
    if (l != null) l else slf4j
  }

  def error(args: LoggingParameter*) {
    val l = actualLogger
    if (l.isErrorEnabled)
      l.error(LoggingParameter.safeFormat(args: _*))
  }

  def debug(args: LoggingParameter*) {
    val l = actualLogger
    if (l.isDebugEnabled)
      l.debug(LoggingParameter.safeFormat(args: _*))
  }

  def info(args: LoggingParameter*) {
    val l = actualLogger
    if (l.isInfoEnabled) 
      l.info(LoggingParameter.safeFormat(args: _*))
  }

  def warn(args: LoggingParameter*) {
    val l = actualLogger
    if (l.isWarnEnabled) 
      l.warn(LoggingParameter.safeFormat(args: _*))
  }
}
