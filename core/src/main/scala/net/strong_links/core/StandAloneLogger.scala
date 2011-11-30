package net.strong_links.core

class StandAloneLogger {

  private def emit(what: String, msg: String) = Console.err.println("[_] _" << (what, msg))

  def debug(msg: String) = emit("Debug", msg)

  def info(msg: String) = emit("Info", msg)

  def warning(msg: String) = emit("Warning", msg)

  def error(msg: String) = emit("Error", msg)
}
