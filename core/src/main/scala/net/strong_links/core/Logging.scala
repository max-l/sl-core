package net.strong_links.core

import LoggingParameter._

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

  protected def actualLogger = Logging.overrider.getOrElse(defaultLogger)

  protected def fmtParams(params: Seq[LoggingParameter]) = LoggingParameter.safeFormat(params)

  def logError(params: LoggingParameter*) {
    val l = actualLogger
    if (l.isErrorEnabled)
      l.error(fmtParams(params))
  }

  def logError(e: Throwable, withStackTrace: Boolean = true) {
    val l = actualLogger
    if (l.isErrorEnabled)
      l.error(Errors.formatException(e, withStackTrace))
  }

  def logWarn(params: LoggingParameter*) {
    val l = actualLogger
    if (l.isWarnEnabled)
      l.warn(fmtParams(params))
  }

  def logInfo(params: LoggingParameter*) {
    val l = actualLogger
    if (l.isInfoEnabled)
      l.info(fmtParams(params))
  }

  def logDebug(params: LoggingParameter*) {
    val l = actualLogger
    if (l.isDebugEnabled)
      l.debug(fmtParams(params))
  }
}

// Same, but will add a number of prefixes in the front of every logged message. The code is a bit repetitive, but this is 
// so to preserve performance. Logging must have very little overhead when not activated.
trait LoggingPrefixed extends Logging {

  protected val loggingPrefixSeq: Seq[LoggingParameter]

  protected override def fmtParams(params: Seq[LoggingParameter]) = safeFormat(concat(loggingPrefixSeq, params))

  override def logError(params: LoggingParameter*) {
    val l = actualLogger
    if (l.isErrorEnabled)
      l.error(fmtParams(params))
  }

  override def logWarn(params: LoggingParameter*) {
    val l = actualLogger
    if (l.isWarnEnabled)
      l.warn(fmtParams(params))
  }

  override def logInfo(params: LoggingParameter*) {
    val l = actualLogger
    if (l.isInfoEnabled)
      l.info(fmtParams(params))
  }

  override def logDebug(params: LoggingParameter*) {
    val l = actualLogger
    if (l.isDebugEnabled)
      l.debug(fmtParams(params))
  }
}
