package com.strong_links.core

import java.io.File

trait LoggingParameter {

  def format: String

  def safeFormat: String
}

object LoggingParameter {

  private def removeTrailingDot(s: String) = if (s.endsWith(".")) s.substring(0, s.length - 1) else s

  private def f(params: Seq[LoggingParameter])(fmt: LoggingParameter => String) = {
    params.map(fmt(_)).filter(!_.isEmpty).map(removeTrailingDot(_)).mkString("", "; ", ".")
  }

  def format(params: Seq[LoggingParameter]) = f(params)(_.format)

  def safeFormat(params: Seq[LoggingParameter]) = f(params)(_.safeFormat)

  def handleNull(s: String) = if (s == null) "(null)" else s

  def concat(params: Seq[LoggingParameter], moreParams: Seq[LoggingParameter]): Seq[LoggingParameter] =
    (params.toList ::: moreParams.toList)
}

import LoggingParameter._

class StringLoggingParameter(val s: String) extends LoggingParameter {

  def format = safeFormat

  def safeFormat = handleNull(s)
}

class OptionStringLoggingParameter(val s: Option[String]) extends LoggingParameter {

  def format = safeFormat

  def safeFormat = (s match { case Some(x) => Some(handleNull(x)); case None => None }).toString
}

class PluggedStringLoggingParameter(val ps: PluggedString) extends LoggingParameter {

  def format = ps.format(failsafe = false, quoted = true)

  def safeFormat = ps.format(failsafe = true, quoted = true)
}

class FileLoggingParameter(val f: File) extends LoggingParameter {

  def ps = "_" << f

  def format = ps.format(failsafe = false, quoted = true)

  def safeFormat = ps.format(failsafe = true, quoted = true)
}
