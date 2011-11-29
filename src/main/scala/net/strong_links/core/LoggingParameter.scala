package net.strong_links.core

trait LoggingParameter {
  def format: String
  def safeFormat: String
}

object LoggingParameter {
  def format(params: LoggingParameter*) = params.map(_.format).mkString("; ")
  def safeFormat(params: LoggingParameter*) = params.map(_.safeFormat).mkString("; ")
}

class StringLoggingParameter(s: String) extends LoggingParameter {
  def format = s
  def safeFormat = s
}

class PluggedStringLoggingParameter(ps: PluggedString) extends LoggingParameter {
  def format = ps.format(failsafe = false, quoted = true)
  def safeFormat = ps.format(failsafe = true, quoted = true)
}

class ExceptionLoggingParameter(e: Exception) extends LoggingParameter {
  def format = ("Catched exception: _" << e.getMessage).format(failsafe = false, quoted = true)
  def safeFormat = ("Catched exception: _" << e.getMessage).format(failsafe = true, quoted = true)
}
