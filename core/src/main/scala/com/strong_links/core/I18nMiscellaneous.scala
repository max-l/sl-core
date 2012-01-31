package com.strong_links.core

abstract class GeneralString

class StringGeneralString(s: String) extends GeneralString {
  override def toString = s
}

class I18nGeneralString(i18n: I18n) extends GeneralString {
  override def toString = i18n.toString
}

object I18nUtil {

  // Ensure that each language variation is handled only once.
  def checkUniqueness(codeLocalization: I18nCodeLocalization, localizations: List[I18nLocalization]) {
    (codeLocalization :: localizations).groupBy(_.i18nLanguageKey).toList.find(_._2.length > 1).map(_._1) match {
      case Some(culprit) => Errors.fatal("The localization with a language key _ exists more than once." << culprit)
      case None =>
    }
  }

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

  def compute(msgCtxt: Option[String], msgid: String) = msgCtxt match {
    case None => msgid
    case Some(ctx) => ctx + "\u0000" + msgid
  }

  def computeForCompiler(msgCtxt: Option[String], msgid: String) = msgCtxt match {
    case None => msgid
    case Some(ctx) => ctx + "\\u0000" + msgid
  }

  def computeForHuman(msgCtxt: Option[String], msgid: String): String = msgCtxt match {
    case None => "msgid _" <<< msgid
    case Some(ctx) => "msgctxt _ and msgid _" <<< (ctx, msgid)
  }
}

object I18nPackageCatalog {
  def apply(packageName: String, codeLanguageKey: String, localizationsStr: String) =
    new I18nConfig(packageName, codeLanguageKey, localizationsStr).toCatalog
}