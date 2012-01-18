package net.strong_links.core

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

  private lazy val defaultLogger: Logging.GenericLogger = org.slf4j.LoggerFactory.getLogger(this.getClass)

  private def actualLogger = Logging.overrider.getOrElse(defaultLogger)

  def logError(params: LoggingParameter*) {
    val l = actualLogger
    if (l.isErrorEnabled)
      l.error(LoggingParameter.safeFormat(params))
  }

  def logError(e: Throwable, withStackTrace: Boolean = true) {
    val l = actualLogger
    if (l.isErrorEnabled)
      l.error(Errors.formatException(e, withStackTrace))
  }

  def logWarn(params: LoggingParameter*) {
    val l = actualLogger
    if (l.isWarnEnabled)
      l.warn(LoggingParameter.safeFormat(params))
  }

  def logInfo(params: LoggingParameter*) {
    val l = actualLogger
    if (l.isInfoEnabled)
      l.info(LoggingParameter.safeFormat(params))
  }

  def logDebug(params: LoggingParameter*) {
    val l = actualLogger
    if (l.isDebugEnabled)
      l.debug(LoggingParameter.safeFormat(params))
  }
}
