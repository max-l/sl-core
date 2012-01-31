package com.strong_links.core

class I18nCatalog(packageName: String, i18nKnownLocalization: I18nKnownLocalization,
  masters: Seq[I18nConfigLocalization] = Nil, subs: Seq[I18nConfigLocalization] = Nil) {

  private[core] val codeUsePlural = i18nKnownLocalization.rule
  private[core] val codeKey = i18nKnownLocalization.i18nLocale.key

  private[core] val packageNameSegments = I18nConfig.toPackageSegments(packageName)

  private[core] val map = makeMap

  def makeLocalization(i18nConfigLocalization: I18nConfigLocalization, parent: Option[I18nLocalization]) =
    new I18nLocalization(packageName, packageNameSegments, i18nConfigLocalization, parent)

  private def makeMap = {
    val t = scala.collection.mutable.Map[String, I18nLocalization]()
    for (m <- masters) {
      m.parentLocale match {
        case None =>
        case Some(pl) => Errors.fatal("Master config localization _ has a parent _." << (m, pl))
      }
      t += (m.i18nLocale.key -> makeLocalization(m, None))
    }
    for (s <- subs) {
      val p = s.parentLocale match {
        case None => Errors.fatal("Sub config localization _ does not have a parent." << s)
        case Some(pl) => pl
      }
      val parent = t.get(p.i18nLocale.key)
      if (parent == None)
        Errors.fatal("Parent localization _ not found for sub config localization _." << (p.i18nLocale.key, s))
      t += (s.i18nLocale.key -> makeLocalization(s, parent))
    }
    t.toMap
  }

  // Flag for no localizations
  val noLocalizations = map.isEmpty
}

protected class I18n(catalog: I18nCatalog, msgCtxt: String, msgid: String, msgidPlural: String, n: Int) {

  lazy val key = if (msgCtxt == null) msgid else (msgCtxt + "\u0000" + msgid).intern

  def toString(i18nLocale: I18nLocale): String = {

    def default = {
      val x = if (n == Int.MaxValue) msgid else if (catalog.codeUsePlural(n)) msgidPlural else msgid
      if (x == null)
        Errors.fatal("Default translation failed on _, _, _." << (msgCtxt, msgid, msgidPlural))
      x
    }

    if (catalog.noLocalizations || (i18nLocale.key eq catalog.codeKey))
      default
    else {
      val i18nLocalization = catalog.map.getOrElse(i18nLocale.key, null)
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

  override def toString = toString(userI18nLocale.getOrElse(I18nLocale.system))

  def <<(args: Any*) = new PluggedI18n(this, Some(args), false)

  def <<<(args: Any*) = new PluggedI18n(this, Some(args), true)
}

class PluggedI18n(i18n: I18n, args: Option[Seq[Any]], quotedDefault: Boolean) {

  override def toString = toString(false, quotedDefault)

  def toString(failsafe: Boolean, quoted: Boolean) = PluggedArguments.format(i18n.toString, args, failsafe, quoted)
}

