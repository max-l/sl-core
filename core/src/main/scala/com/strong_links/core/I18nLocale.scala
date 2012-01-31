package com.strong_links.core

import java.util.Locale

object I18nLocale {

  def apply(locale: Locale) = new I18nLocale(locale)

  def sort(a: I18nLocale, b: I18nLocale) = (a compare b) < 0

  def from(languageKey: String) = I18nLocale(Util.split(languageKey, '_') match {
    case List(language, country, variant) => new Locale(language, country, variant)
    case List(language, country) => new Locale(language, country)
    case List(language) => new Locale(language)
    case _ => Errors.fatal("Invalid localization _." << languageKey)
  })

  // System language key at startup. This is *not* expected to change in a server environment.
  val system = apply(Locale.getDefault)
}

// A helper I18nLocale class to increase the basic functionality of the Java Locale class.
private[core] class I18nLocale(val locale: Locale) {

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

