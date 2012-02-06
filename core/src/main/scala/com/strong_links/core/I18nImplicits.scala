package com.strong_links.core

import java.util.Locale

trait I18nImplicits {

  implicit def stringToPluggableString(s: String) = new PluggableString(s)

  implicit def pluggedStringToString(pluggedString: PluggedString) = pluggedString.f

  implicit def stringToStringGeneralString(s: String): GeneralString = new StringGeneralString(s)

  implicit def i18nToI18nGeneralString(i18n: I18n): GeneralString = new I18nGeneralString(i18n)

  def I18n[S <: String](msgid: S)(implicit catalog: I18nCatalog) =
    new PluggableI18n(catalog, null, msgid, msgid, Int.MaxValue)

  def I18nCtxt[S <: String](msgCtxt: S, msgid: S)(implicit catalog: I18nCatalog) =
    new PluggableI18n(catalog, msgCtxt, msgid, msgid, Int.MaxValue)

  def I18nPlural[S <: String](msgid: S, msgidPlural: S, n: Int)(implicit catalog: I18nCatalog) =
    new PluggableI18n(catalog, null, msgid, msgidPlural, n)

  def I18nPlural[S <: String](msgid: S, n: Int)(implicit catalog: I18nCatalog) =
    new PluggableI18n(catalog, null, msgid, msgid, n)

  def I18nPluralCtxt[S <: String](msgCtxt: S, msgid: S, msgPlural: S, n: Int)(implicit catalog: I18nCatalog) =
    new PluggableI18n(catalog, msgCtxt, msgid, msgPlural, n)

  def I18nPluralCtxt[S <: String](msgCtxt: S, msgid: S, n: Int)(implicit catalog: I18nCatalog) =
    new PluggableI18n(catalog, msgCtxt, msgid, msgid, n)

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
}