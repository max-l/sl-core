package net.strong_links.core

trait LoggingParameter {
  def format: String
  def safeFormat: String
}

object LoggingParameter {
  private def f(params: LoggingParameter*)(fmt: LoggingParameter => String) = params.map(fmt(_)).filter(!_.isEmpty).mkString("; ")
  def format(params: LoggingParameter*) = f(params: _*)(_.format)
  def safeFormat(params: LoggingParameter*) = f(params: _*)(_.safeFormat)
}

class StringLoggingParameter(s: String) extends LoggingParameter {
  def format = s
  def safeFormat = format
}

class OptionStringLoggingParameter(s: Option[String]) extends LoggingParameter {
  def format = s match { case None => ""; case Some(x) => x }
  def safeFormat = format
}

class PluggedStringLoggingParameter(ps: PluggedString) extends LoggingParameter {
  def format = ps.format(failsafe = false, quoted = true)
  def safeFormat = ps.format(failsafe = true, quoted = true)
}

class ExceptionLoggingParameter(e: Exception) extends LoggingParameter {
  def format = ("Catched exception: _" << e.getMessage).format(failsafe = false, quoted = true)
  def safeFormat = ("Catched exception: _" << e.getMessage).format(failsafe = true, quoted = true)
}
