package com.strong_links.core

class I18nCatalog(val i18nConfig: I18nConfig) {

  private var map = Map[String, I18nLocalization]()

  def loadChainedLocalization(chain: List[Option[I18nLocale]]): Option[I18nLocalization] = chain match {
    case List(None) => None
    case Some(head) :: rest => Some(new I18nLocalization(this, head, loadChainedLocalization(rest)))
    case _ => Errors.fatal("Invalid chain _." << chain)
  }

  def getLocalizationFor(i18nLocale: I18nLocale) = map.getOrElse(i18nLocale.key, {
    val chain = i18nConfig.getLocalizations(i18nLocale).spy
    loadChainedLocalization(chain) match {
      case Some(x) => map += (i18nLocale.key -> x); x
      case None => Errors.fatal("No localization loaded for locale _." << i18nLocale)
    }
  })

  // Helpers for faster translation.
  private val codeUsePlural = i18nConfig.i18nCodeLocalization.rule
  private val codeKey = i18nConfig.i18nCodeLocalization.i18nLocale.key

  def translate(i18n: I18n, i18nLocale: I18nLocale) = {

    import i18n._

    def default = {
      val x = if (n == Int.MaxValue) msgid else if (codeUsePlural(n)) msgidPlural else msgid
      if (x == null)
        Errors.fatal("Default translation failed with null on _, _, _." << (msgCtxt, msgid, msgidPlural))
      x
    }

    if (i18nLocale.key eq codeKey)
      default
    else {
      val i18nLocalization = getLocalizationFor(i18nLocale)
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

