package net.strong_links.core

class StandAloneLogger {

  private def emit(what: String, msg: String): Unit = Console.err.println("[_] _" << (what, msg))

  def isErrorEnabled(): Boolean = true

  def isInfoEnabled(): Boolean = true

  def isWarnEnabled(): Boolean = true

  def isDebugEnabled(): Boolean = true

  def debug(msg: String): Unit = emit("Debug", msg)

  def info(msg: String): Unit = emit("Info", msg)

  def warn(msg: String): Unit = emit("Warning", msg)

  def error(msg: String): Unit = emit("Error", msg)
}
