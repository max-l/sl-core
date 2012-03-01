package com.strong_links.core

import LoggingParameter._
import java.net.URLClassLoader

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

  class BasicLogger(c: Class[_]) {

    private val className = c.getSimpleName

    private var level: Int = Debug

    def setLevelDebug { level = Debug }
    def setLevelInfo { level = Info }
    def setLevelWarn { level = Warn }
    def setLevelError { level = Error }

    def isErrorEnabled(): Boolean = level <= Error
    def isWarnEnabled(): Boolean = level <= Warn
    def isInfoEnabled(): Boolean = level <= Info
    def isDebugEnabled(): Boolean = level <= Debug

    def log(what: String, message: String) =
      Console.err.println(Util.nowForLogging + " " + what + " " + className + " - " + message)

    def warn(s: String) = log("WARN", s)
    def error(s: String) = log("ERROR", s)
    def info(s: String) = log("INFO", s)
    def debug(s: String) = log("DEBUG", s)
  }

  type LoggerCreator = Class[_] => GenericLogger

  private var loggerCreator: LoggerCreator = c => new BasicLogger(c)

  def setLogger(c: LoggerCreator) { loggerCreator = c }
  
  def resetLogger {
    loggerCreator = c => new BasicLogger(c)
  }
}

trait Logging {

  private val logger = Logging.loggerCreator(this.getClass)

  protected def fmtParams(params: Seq[LoggingParameter]) = LoggingParameter.safeFormat(params)

  def logError(params: LoggingParameter*) {
    if (logger.isErrorEnabled)
      logger.error(fmtParams(params))
  }

  def logError(e: Throwable, withStackTrace: Boolean = true) {
    if (logger.isErrorEnabled)
      logger.error(Errors.formatException(e, withStackTrace))
  }

  def logWarn(params: LoggingParameter*) {
    if (logger.isWarnEnabled)
      logger.warn(fmtParams(params))
  }

  def logInfo(params: LoggingParameter*) {
    if (logger.isInfoEnabled)
      logger.info(fmtParams(params))
  }

  def logDebug(params: LoggingParameter*) {
    if (logger.isDebugEnabled)
      logger.debug(fmtParams(params))
  }
}
