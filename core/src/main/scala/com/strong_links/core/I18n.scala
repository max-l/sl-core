package com.strong_links.core

protected class I18n(catalog: I18nCatalog, val msgCtxt: String, val msgid: String, val msgidPlural: String, val n: Int) {

  lazy val key = if (msgCtxt == null) msgid else (msgCtxt + "\u0000" + msgid).intern

  def toString(i18nLocale: I18nLocale) = catalog.translate(this, i18nLocale)

  override def toString = toString(I18nLocale.systemCurrent)

  def f(implicit i18nLocale: I18nLocale) = toString(i18nLocale)

  def <<(args: Any*) = new PluggedI18n(this, Some(args), false)

  def <<<(args: Any*) = new PluggedI18n(this, Some(args), true)
}

class PluggedI18n(i18n: I18n, args: Option[Seq[Any]], quotedDefault: Boolean) {

  override def toString = toString(I18nLocale.systemCurrent, false, quotedDefault)

  def toString(i18nLocale: I18nLocale): String = toString(i18nLocale, false, quotedDefault)

  def toString(i18nLocale: I18nLocale, failsafe: Boolean, quoted: Boolean): String =
    PluggedArguments.format(i18n.toString(i18nLocale), args, failsafe, quoted)

  def f(implicit i18nLocale: I18nLocale) = toString(i18nLocale)
}

