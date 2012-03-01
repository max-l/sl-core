package com.strong_links.core

trait I18n {

  def key: String

  def f(implicit i18nLocale: I18nLocale): String
}