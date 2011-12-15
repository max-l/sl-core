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

  def handleNull(s: String) = if (s == null) "(null)" else s
}

import LoggingParameter._

class StringLoggingParameter(s: String) extends LoggingParameter {

  def format = handleNull(s)

  def safeFormat = format
}

class OptionStringLoggingParameter(s: Option[String]) extends LoggingParameter {

  def format = (s match { case Some(null) => Some("(null)"); case _ => s }).toString

  def safeFormat = format
}

class PluggedStringLoggingParameter(ps: PluggedString) extends LoggingParameter {

  def format = ps.format(failsafe = false, quoted = true)

  def safeFormat = ps.format(failsafe = true, quoted = true)
}

class ExceptionLoggingParameter(e: Exception) extends LoggingParameter {

  def stackTrace = e.getStackTrace

  def format = (e match { case e: SystemException => ""; case _ => OS.getFinalClassName(e) + " exception: " }) + handleNull(e.getMessage)

  def safeFormat = format
}
