package com.strong_links

import java.util.Locale
import java.io.File

package object core {

  implicit val i18nLocaleComparer = new Ordering[I18nLocale] {
    def compare(a: I18nLocale, b: I18nLocale): Int = a compare b
  }

  implicit def i18nLocaleToLocale(i18nLocale: I18nLocale): Locale =
    i18nLocale.locale

  implicit def i18nStockToLocale(i18nStock: I18nStock): Locale =
    i18nStock.i18nLocale.locale

  implicit def i18nStockToI18nLocale(i18nStock: I18nStock): I18nLocale =
    i18nStock.i18nLocale

  implicit def seqI18nLocaleToSeqLocale(seq: Seq[I18nLocale]): Seq[Locale] =
    seq.map(_.locale)

  implicit def seqI18nStockToSeqLocale(seq: Seq[I18nStock]): Seq[Locale] =
    seq.map(_.i18nLocale.locale)

  implicit def mapI18nLocaleToMapLocale(map: Map[I18nLocale, I18nLocale]): Map[Locale, Locale] =
    map.map(e => (e._1.locale, e._2.locale))

  implicit def mapI18nStockToMapLocale(map: Map[I18nStock, I18nStock]): Map[Locale, Locale] =
    map.map(e => (e._1.i18nLocale.locale, e._2.i18nLocale.locale))

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

  class wrapper[R](r: R) {
    def spy = {
      Console.err.println("Spy: _" << r)
      r
    }
  }

  implicit def anyToWrapper[R](r: R) = new wrapper[R](r)
}