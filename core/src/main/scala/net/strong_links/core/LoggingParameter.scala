package net.strong_links.core

trait LoggingParameter {
  def format: String
  def safeFormat: String
}

object LoggingParameter {
  def removeTrailingDot(s: String) = if (s.endsWith(".")) s.substring(0, s.length - 1) else s
  private def f(params: LoggingParameter*)(fmt: LoggingParameter => String) = {
    params.map(fmt(_)).map(s => if (s == null) "null" else s).filter(!_.isEmpty).map(removeTrailingDot(_)).mkString("", "; ", ".")
  }
  def format(params: LoggingParameter*) = f(params: _*)(_.format)
  def safeFormat(params: LoggingParameter*) = f(params: _*)(_.safeFormat)
}

class StringLoggingParameter(s: String) extends LoggingParameter {
  def format = s
  def safeFormat = format
}

class OptionStringLoggingParameter(s: Option[String]) extends LoggingParameter {
  def format = s match { case null => "null" case None => ""; case Some(x) => x }
  def safeFormat = format
}

class PluggedStringLoggingParameter(ps: PluggedString) extends LoggingParameter {
  def format = ps.format(failsafe = false, quoted = true)
  def safeFormat = ps.format(failsafe = true, quoted = true)
}

class ExceptionLoggingParameter(e: Exception) extends LoggingParameter {
  private def message = if (e.getMessage == null) "(No message)" else e.getMessage
  def format = e match {
    case _: StrongLinksException =>
      message
    case _: Exception =>
      val dumpBody = Util.split(e.getStackTraceString.trim).map("* " + _)
      val niceDump = "(Original stack trace start)" +: dumpBody :+ "(Original stack trace end)"
      OS.getFinalClassName(e) + " exception: " + message + "\n" + niceDump.mkString("\n")
  }
  def safeFormat = format
}
