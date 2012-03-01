package com.strong_links.core

import java.util.Locale

trait I18nImplicits {

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