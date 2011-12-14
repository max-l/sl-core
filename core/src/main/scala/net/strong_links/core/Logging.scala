package net.strong_links.core

import org.slf4j.{LoggerFactory,Logger => SLF4JLogger}

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

  private object Overrider extends ThreadLocalStack[GenericLogger]  

  def using[R](logger: GenericLogger) (code: => R): R =
    Overrider.using(logger)(code)
}

trait Logging {

  private lazy val slf4j = LoggerFactory.getLogger(this.getClass)

  private def actualLogger = {
    val l = Logging.Overrider.unsafeGet
    if(l != null) l
    else slf4j
  }

  def error(args: LoggingParameter*) {
    val l = actualLogger
    if(l.isErrorEnabled) args.foreach(a => l.error(a.safeFormat))
  }
  
  def debug(args: LoggingParameter*) {
    val l = actualLogger
    if(l.isDebugEnabled) args.foreach(a => l.debug(a.safeFormat))
  }
  
  def info(args: LoggingParameter*) {
    val l = actualLogger
    if(l.isInfoEnabled) args.foreach(a => l.info(a.safeFormat))
  }

  def warn(args: LoggingParameter*) {
    val l = actualLogger
    if(l.isWarnEnabled) args.foreach(a => l.warn(a.safeFormat))
  }
}
