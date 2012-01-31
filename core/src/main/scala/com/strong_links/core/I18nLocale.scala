package com.strong_links.core

import java.util.Locale

object I18nLocale {

  def apply(locale: Locale) = new I18nLocale(locale)

  def sort(a: I18nLocale, b: I18nLocale) = (a compare b) < 0

  def from(languageKey: String) = {
    val segments = Util.split(languageKey, '_')
    val locale = segments match {
      case List(language, country, variant) =>
        new Locale(language, country, variant)
      case List(language, country) =>
        new Locale(language, country)
      case List(language) =>
        new Locale(language)
      case _ =>
        Errors.fatal("Invalid localization _." << languageKey)
    }
    I18nLocale(locale)
  }

  // System language key at startup. This is *not* expected to change in a server environment.
  val system = apply(Locale.getDefault)
}

// A helper I18nLocale class to increase the basic functionality of the Java Locale class.
// We also insure that the passed Locale is valid. The Java constructor allows any kind
// of junk to be entered in the Locale.
private[core] class I18nLocale(_locale: Locale) {

  val locale = Errors.trap("Invalid input Locale _." << _locale) {
    def normalize(s: String) = if (s == null) "" else s.trim
    def checkAllLetters(s: String) {
      if (s.exists(!_.isLetter)) Errors.fatal("Invalid characters found in _; expected only letters." << s)
    }
    val language = normalize(_locale.getLanguage).toLowerCase
    if (language == "")
      Errors.fatal("No language found.")
    checkAllLetters(language)
    val country = normalize(_locale.getCountry).toUpperCase
    checkAllLetters(country)
    val variant = normalize(_locale.getVariant)
    new Locale(language, country, variant)
  }

  val key = locale.toString.intern

  private def d(s: String) = s match {
    case null | "" => false
    case _ => true
  }

  val levels = {
    def x(s: String) = if (d(s)) 1 else 0
    x(locale.getLanguage) + x(locale.getCountry) + x(locale.getVariant)
  }

  override def toString = key

  override def equals(that: Any) = that match {
    case x: I18nLocale => key == x.key
    case _ => false
  }

  override def hashCode = key.hashCode

  def compare(that: I18nLocale): Int = (this.levels compare that.levels) match {
    case 0 => this.key compare that.key
    case x => x
  }

  def down = if (d(locale.getVariant))
    Some(I18nLocale(new Locale(locale.getLanguage, locale.getCountry)))
  else if (d(locale.getCountry))
    Some(I18nLocale(new Locale(locale.getLanguage)))
  else
    None
}

