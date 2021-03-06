package com.strong_links.core

import java.util.Locale

object I18nUtil {

  def validate(os: Option[String], what: String) = {
    os match {
      case None =>
      case Some(s) =>
        if (s.trim != s)
          Errors.fatal("The _ string _ has leading or trailing whitespace." << (what, s))
        if (s == "")
          Errors.fatal("Empty _ string." << what)
    }
  }

  def compute(msgCtxt: String, msgid: String) = if (msgCtxt == null) msgid else msgCtxt + "\u0000" + msgid

  def compute(msgCtxt: Option[String], msgid: String): String = compute(msgCtxt.getOrElse(null), msgid)

  def computeForCompiler(msgCtxt: Option[String], msgid: String) = msgCtxt match {
    case None      => msgid
    case Some(ctx) => ctx + "\\u0000" + msgid
  }

  def computeForHuman(msgCtxt: Option[String], msgid: String): String = msgCtxt match {
    case None      => "msgid _" <<< msgid
    case Some(ctx) => "msgctxt _ and msgid _" <<< (ctx, msgid)
  }
}
