package com.strong_links.core

class I18nCatalog(packageName: String, i18nKnownLocalization: I18nKnownLocalization,
  masters: Seq[I18nConfigLocalization] = Nil, subs: Seq[I18nConfigLocalization] = Nil) {

  private val packageNameSegments = I18nConfig.toPackageSegments(packageName)

  private val codeUsePlural = i18nKnownLocalization.rule
  private val codeKey = i18nKnownLocalization.i18nLocale.key

  private val map = {

    val t = scala.collection.mutable.Map[String, I18nLocalization]()

    def addTuple(i18nConfigLocalization: I18nConfigLocalization, parent: Option[I18nLocalization]) =
      t += (i18nConfigLocalization.i18nLocale.key -> new I18nLocalization(packageName, packageNameSegments, i18nConfigLocalization, parent))

    for (m <- masters)
      m.parentLocale match {
        case None =>
          addTuple(m, None)
        case Some(pl) =>
          Errors.fatal("Master config localization _ has a parent _." << (m, pl))
      }

    for (s <- subs)
      (s.parentLocale match {
        case None =>
          Errors.fatal("Sub config localization _ does not have a parent." << s)
        case Some(pl) =>
          t.get(pl.i18nLocale.key)
      }) match {
        case None =>
          Errors.fatal("Parent localization _ not found for sub config localization _." << (s.parentLocale, s))
        case parent =>
          addTuple(s, parent)
      }

    t.toMap
  }

  val noLocalizations = map.isEmpty

  def translate(i18n: I18n, i18nLocale: I18nLocale) = {

    import i18n._

    def default = {
      val x = if (n == Int.MaxValue) msgid else if (codeUsePlural(n)) msgidPlural else msgid
      if (x == null)
        Errors.fatal("Default translation failed on _, _, _." << (msgCtxt, msgid, msgidPlural))
      x
    }

    if (noLocalizations || (i18nLocale.key eq codeKey))
      default
    else {
      val i18nLocalization = map.getOrElse(i18nLocale.key, null)
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
}

