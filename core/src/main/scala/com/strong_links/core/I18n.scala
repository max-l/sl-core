package com.strong_links.core

class I18nCatalog(packageSegments: List[String], codeLocalization: I18nCodeLocalization, localizations: List[I18nLocalization]) {

  val codeUsePlural = codeLocalization.usePluralRule
  val codeI18nLanguageKey = codeLocalization.i18nLanguageKey

  // Make sure each localization appears only once.
  I18nUtil.checkUniqueness(codeLocalization, localizations)

  // Try to get any string to force the dynamic class loading.
  localizations.foreach(_.gettext("\uFFFF"))

  // Create a map to find a localization according to its language key represented (as a single string).
  private[core] val map = localizations.map(L => (L.i18nLanguageKey.string, L)).toMap

  // Flag for no localizations
  val noLocalizations = localizations.isEmpty
}

protected class I18n(catalog: I18nCatalog, msgCtxt: String, msgid: String, msgidPlural: String, n: Int) {

  lazy val key = if (msgCtxt == null) msgid else (msgCtxt + "\u0000" + msgid).intern

  def toString(i18nLanguageKey: I18nLanguageKey): String = {

    def default = {
      val x = if (n == Int.MaxValue) msgid else if (catalog.codeUsePlural(n)) msgidPlural else msgid
      if (x == null)
        Errors.fatal("Default translation failed on _, _, _." << (msgCtxt, msgid, msgidPlural))
      x
    }

    if (catalog.noLocalizations || (i18nLanguageKey.string eq catalog.codeI18nLanguageKey.string))
      default
    else {
      val i18nLocalization = catalog.map.getOrElse(i18nLanguageKey.string, null)
      if (i18nLocalization == null)
        default
      else {
        val translation = if (n == Int.MaxValue)
          i18nLocalization.gettext(key)
        else
          i18nLocalization.ngettext(key, n)
        if (translation == null)
          default
        else
          translation
      }
    }
  }

  override def toString = toString(userI18nLanguageKey.getOrElse(I18nLanguageKey.system))

  def <<(args: Any*) = new PluggedI18n(this, Some(args), false)

  def <<<(args: Any*) = new PluggedI18n(this, Some(args), true)
}

class PluggedI18n(i18n: I18n, args: Option[Seq[Any]], quotedDefault: Boolean) {

  override def toString = toString(false, quotedDefault)

  def toString(failsafe: Boolean, quoted: Boolean) = PluggedArguments.format(i18n.toString, args, failsafe, quoted)
}

