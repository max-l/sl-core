package com.strong_links.core

class I18nCatalog(val i18nConfig: I18nConfig) {

  private var cache = Map[String, I18nLocalization]()

  def i18n[S <: String](msgid: S) =
    new PluggableI18n(this, null, msgid, msgid, Int.MaxValue)

  def i18nCtxt[S <: String](msgCtxt: S, msgid: S) =
    new PluggableI18n(this, msgCtxt, msgid, msgid, Int.MaxValue)

  def i18nPlural[S <: String](msgid: S, msgidPlural: S, n: Int) =
    new PluggableI18n(this, null, msgid, msgidPlural, n)

  def i18nPlural[S <: String](msgid: S, n: Int) =
    new PluggableI18n(this, null, msgid, msgid, n)

  def i18nPluralCtxt[S <: String](msgCtxt: S, msgid: S, msgPlural: S, n: Int) =
    new PluggableI18n(this, msgCtxt, msgid, msgPlural, n)

  def i18nPluralCtxt[S <: String](msgCtxt: S, msgid: S, n: Int) =
    new PluggableI18n(this, msgCtxt, msgid, msgid, n)

  def loadChainedLocalization(chain: List[Option[I18nLocale]]): I18nLocalization = chain match {
    case List(None)         => null
    case Some(head) :: rest => new I18nLocalization(this, head, loadChainedLocalization(rest))
    case _                  => Errors.fatal("Invalid chain _." << chain)
  }

  def getCachedLocalization(i18nLocale: I18nLocale) = cache.getOrElse(i18nLocale.key, {
    val x = loadChainedLocalization(i18nConfig.getLocalizations(i18nLocale))
    cache += (i18nLocale.key -> x)
    x
  })

  def translate(i18nPluggable: PluggableI18n, i18nLocale: I18nLocale) = {

    import i18nPluggable._

    def default = if (n == Int.MaxValue) msgid else if (i18nConfig.i18nCodeLocalization.rule(n)) msgidPlural else msgid

    def bad(what: LoggingParameter) =
      Errors.fatal(what, "On specifications msgCtxt _, msgid _, msgidPlural _." << (msgCtxt, msgid, msgidPlural))

    // Use default if the required localization does not exist or it the translation done through a localization
    // did not succeed.
    val translation = (getCachedLocalization(i18nLocale) match {
      case null => default
      case loc => (if (n == Int.MaxValue) loc.gettext(key) else loc.ngettext(key, n)) match {
        case null => default
        case x    => x
      }
    })

    if (translation == null)
      bad("Unexpected null translation.")

    // Handle the possible msgCtxt appearing at the start of the string, along with its separator character, '\0000'.
    if (msgCtxt == null)
      translation
    else {
      val startIndex = msgCtxt.length + 1
      if (startIndex >= translation.length)
        Errors.fatal("Inconsistent default translation failed with null on _, _, _." << (msgCtxt, msgid, msgidPlural))
      translation.substring(startIndex)
    }
  }
}

