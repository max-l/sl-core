package com.strong_links.core

import java.util.Locale

object I18nLanguageKey {

  def from(language: String, country: Option[String]): I18nLanguageKey = {
    def normalize(s: String) = if (s == null) "" else s.trim
    def normalizedLanguage = normalize(language).toLowerCase
    if (normalizedLanguage == "")
      Errors.fatal("No language found.")
    val normalizedCountry = country match {
      case None => None
      case Some(c) => normalize(c).toUpperCase match {
        case "" => Errors.fatal("No country found.")
        case x => Some(x)
      }
    }
    new I18nLanguageKey(normalizedLanguage, normalizedCountry)
  }

  def from(locale: Locale): I18nLanguageKey = Errors.trap("Invalid locale _" << locale) {
    from(locale.getLanguage, locale.getCountry match { case null => None; case "" => None; case x => Some(x) })
  }

  def from(languageKey: String): I18nLanguageKey = Errors.trap("Invalid language key _" << languageKey) {
    if (languageKey.contains('_')) {
      val (a, b) = Util.splitTwo(languageKey, '_')
      from(a, Some(b))
    } else
      from(languageKey, None)
  }

  // System language key at startup. This is *not* expected to change in a server environment.
  lazy val system = from(Locale.getDefault)
}

case class I18nLanguageKey(language: String, country: Option[String]) {

  // Precompute its string representation, which will exist only once in the server.
  val string = (country match {
    case None => language
    case Some(c) => language + "_" + c
  }).intern

  override def toString = string

  def compare(that: I18nLanguageKey): Int = this.string compare that.string
}

