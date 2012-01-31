package com.strong_links

import java.util.Locale
import java.io.File

package object core {

  implicit def stringToPluggableString(s: String) = {
    new PluggableString(s)
  }

  implicit def pluggedStringToString(ps: PluggedString) = {
    ps.toString
  }

  implicit def stringToStringLoggingParameter(s: String) = {
    new StringLoggingParameter(s)
  }

  implicit def optionStringToStringLoggingParameter(s: Option[String]) = {
    new OptionStringLoggingParameter(s)
  }

  implicit def pluggedStringToPluggedStringLoggingParameter(ps: PluggedString) = {
    new PluggedStringLoggingParameter(ps)
  }

  implicit def fileToFileLoggingParameter(f: File) = {
    new FileLoggingParameter(f)
  }

  object userI18nLocale extends ThreadLocalStack[I18nLocale]

  implicit def stringToStringGeneralString(s: String): GeneralString = {
    new StringGeneralString(s)
  }

  implicit def i18nToI18nGeneralString(i18n: I18n): GeneralString = {
    new I18nGeneralString(i18n)
  }

  implicit def FileToBetterFile(file: File) = new BetterFile(file)

  // I18n objects creation with specific methods. Note that in order to improve performance,
  // "null" and "Int.MaxValue" are used to minimize the creation of "Option" objects .
  def I18n(msgid: String)(implicit catalog: I18nCatalog) =
    new I18n(catalog, null, msgid, msgid, Int.MaxValue)

  def I18nCtxt(msgCtxt: String, msgid: String)(implicit catalog: I18nCatalog) =
    new I18n(catalog, msgCtxt, msgid, msgid, Int.MaxValue)

  def I18nPlural(msgid: String, msgidPlural: String, n: Int)(implicit catalog: I18nCatalog) =
    new I18n(catalog, null, msgid, msgidPlural, n)

  def I18nPlural(msgid: String, n: Int)(implicit catalog: I18nCatalog) =
    new I18n(catalog, null, msgid, msgid, n)

  def I18nPluralCtxt(msgCtxt: String, msgid: String, msgPlural: String, n: Int)(implicit catalog: I18nCatalog) =
    new I18n(catalog, msgCtxt, msgid, msgPlural, n)

  def I18nPluralCtxt(msgCtxt: String, msgid: String, n: Int)(implicit catalog: I18nCatalog) =
    new I18n(catalog, msgCtxt, msgid, msgid, n)

  implicit def doubleToDoubleFormatter(d: Double) = new DoubleFormatter(d)
}