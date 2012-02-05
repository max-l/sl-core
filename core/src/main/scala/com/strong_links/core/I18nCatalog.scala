package com.strong_links.core

class I18nCatalog(val i18nConfig: I18nConfig) {

  private var cache = Map[String, I18nLocalization]()

  def loadChainedLocalization(chain: List[Option[I18nLocale]]): I18nLocalization = chain match {
    case List(None) => null
    case Some(head) :: rest => new I18nLocalization(this, head, loadChainedLocalization(rest))
    case _ => Errors.fatal("Invalid chain _." << chain)
  }

  def getCachedLocalization(i18nLocale: I18nLocale) = cache.getOrElse(i18nLocale.key, {
    val x = loadChainedLocalization(i18nConfig.getLocalizations(i18nLocale))
    cache += (i18nLocale.key -> x)
    x
  })

  def translate(i18n: I18n, i18nLocale: I18nLocale) = {

    import i18n._

    def default = if (n == Int.MaxValue) msgid else if (i18nConfig.i18nCodeLocalization.rule(n)) msgidPlural else msgid

    (getCachedLocalization(i18nLocale) match {
      case null => default
      case loc => (if (n == Int.MaxValue) loc.gettext(key) else loc.ngettext(key, n)) match {
        case null => default
        case translation => translation
      }
    }) match {
      case null => Errors.fatal("Default translation failed with null on _, _, _." << (msgCtxt, msgid, msgidPlural))
      case translation => translation
    }
  }
}

