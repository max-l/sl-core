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

  private lazy val defaultLogger: Logging.GenericLogger = new StandAloneLogger
  //    try org.slf4j.LoggerFactory.getLogger(this.getClass) catch {
  //      case e: NoClassDefFoundError => new StandAloneLogger
  //      case e: Exception => Errors.fatal("Unexpected exception trying to load sl4j logger.", e)
  //   }

  private def actualLogger = {
    val l = Logging.overrider.unsafeGet
    if (l != null) l else defaultLogger
  }

  def logError(params: LoggingParameter*) {
    val l = actualLogger
    if (l.isErrorEnabled)
      l.error(LoggingParameter.safeFormat(params: _*))
  }

  def logDebug(params: LoggingParameter*) {
    val l = actualLogger
    if (l.isDebugEnabled)
      l.debug(LoggingParameter.safeFormat(params: _*))
  }

  def logInfo(params: LoggingParameter*) {
    val l = actualLogger
    if (l.isInfoEnabled)
      l.info(LoggingParameter.safeFormat(params: _*))
  }

  def logWarn(params: LoggingParameter*) {
    val l = actualLogger
    if (l.isWarnEnabled)
      l.warn(LoggingParameter.safeFormat(params: _*))
  }

  def logException(e: Exception, withStackTrace: Boolean) {
    logError(e)
    val at = "at"
    val causedAt = "caused at"
    val max = at.length max causedAt.length
    def dump(label: String, stackTraceElements: Array[StackTraceElement]) =
      stackTraceElements.foreach(ste => logError(label.padTo(max, ' ') + " " + ste))
    if (withStackTrace) {
      dump(at, e.stackTrace)
      e match { case SystemException(ste) => dump(causedAt, ste); case _ => }
    }
  }
}
