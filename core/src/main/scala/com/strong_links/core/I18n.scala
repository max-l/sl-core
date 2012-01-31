package com.strong_links.core

protected class I18n(catalog: I18nCatalog, val msgCtxt: String, val msgid: String, val msgidPlural: String, val n: Int) {

  lazy val key = if (msgCtxt == null) msgid else (msgCtxt + "\u0000" + msgid).intern

  def toString(i18nLocale: I18nLocale) = catalog.translate(this, i18nLocale)

  override def toString = toString(userI18nLocale.getOrElse(I18nLocale.system))

  def <<(args: Any*) = new PluggedI18n(this, Some(args), false)

  def <<<(args: Any*) = new PluggedI18n(this, Some(args), true)
}

class PluggedI18n(i18n: I18n, args: Option[Seq[Any]], quotedDefault: Boolean) {

  override def toString = toString(false, quotedDefault)

  def toString(failsafe: Boolean, quoted: Boolean) = PluggedArguments.format(i18n.toString, args, failsafe, quoted)
}

