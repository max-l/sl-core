package com.strong_links.core

import LoggingParameter._

object Logging {

  val Debug = 1
  val Info = 2
  val Warn = 3
  val Error = 4

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

  class BasicLogger(startLevel: Int) {

    private var _level: Int = startLevel

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

  def defaultLogger = try
    Class.forName("org.slf4j.LoggerFactory").getMethod("getLogger", classOf[Class[_]]).
      invoke(null, this.getClass).asInstanceOf[Logging.GenericLogger]
  catch {
    case _ => new BasicLogger(Debug): Logging.GenericLogger
  }

  private var _logger: Logging.GenericLogger = defaultLogger

  def logger = _logger

  def logger_=(newLogger: Logging.GenericLogger) = {
    if (newLogger == null)
      Errors.badValue(newLogger)
    _logger = newLogger
  }
}

trait Logging {

  import Logging._

  protected def fmtParams(params: Seq[LoggingParameter]) = LoggingParameter.safeFormat(params)

  def logError(params: LoggingParameter*) {
    if (_logger.isErrorEnabled)
      _logger.error(fmtParams(params))
  }

  def logError(e: Throwable, withStackTrace: Boolean = true) {
    if (_logger.isErrorEnabled)
      _logger.error(Errors.formatException(e, withStackTrace))
  }

  def logWarn(params: LoggingParameter*) {
    if (_logger.isWarnEnabled)
      _logger.warn(fmtParams(params))
  }

  def logInfo(params: LoggingParameter*) {
    if (_logger.isInfoEnabled)
      _logger.info(fmtParams(params))
  }

  def logDebug(params: LoggingParameter*) {
    if (_logger.isDebugEnabled)
      _logger.debug(fmtParams(params))
  }
}
