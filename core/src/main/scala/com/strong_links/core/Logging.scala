package com.strong_links.core

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

object BasicLogger {

  var preferred = false

  private val Debug = 1
  private val Info = 2
  private val Warn = 3
  private val Error = 4

  private var _level: Int = Debug

  def setLevelDebug { _level = Debug }
  def setLevelInfo { _level = Info }
  def setLevelWarn { _level = Warn }
  def setLevelError { _level = Error }

  def isErrorEnabled(): Boolean = _level <= Error
  def isWarnEnabled(): Boolean = _level <= Warn
  def isInfoEnabled(): Boolean = _level <= Info
  def isDebugEnabled(): Boolean = _level <= Debug

  def log(level: String, message: String) =
    Console.err.println(Util.nowForLogging + " " + level + " " + message)

  def warn(s: String) = log("WARN", s)
  def error(s: String) = log("ERROR", s)
  def info(s: String) = log("INFO", s)
  def debug(s: String) = log("DEBUG", s)
}

trait Logging {

  // We favorize slf4j, but if it is not in scope, we use our basic logger.
  private lazy val defaultLogger: Logging.GenericLogger =
    if (BasicLogger.preferred)
      BasicLogger: Logging.GenericLogger
    else try
      Class.forName("org.slf4j.LoggerFactory").getMethod("getLogger", classOf[Class[_]]).
        invoke(null, this.getClass).asInstanceOf[Logging.GenericLogger]
    catch {
      case _ => BasicLogger: Logging.GenericLogger
    }

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

