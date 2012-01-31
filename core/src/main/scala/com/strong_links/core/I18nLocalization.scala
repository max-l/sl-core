package com.strong_links.core

class I18nLocalization(val packageSegments: List[String], val i18nLanguageKey: I18nLanguageKey, val parent: Option[I18nLocalization]) {

  def packageName = packageSegments.mkString(".")

  // Keep an internal less safe reference to the parent as this will be faster at run-time.
  private val _parent = parent match { case None => null; case Some(p) => p }

  override def toString = parent match {
    case None => i18nLanguageKey.string
    case Some(p) => i18nLanguageKey + ":" + p.i18nLanguageKey
  }

  def className = (i18nLanguageKey :: packageSegments).mkString("_")

  def fqn = (packageSegments :+ className).mkString(".")

  private lazy val dynamicClass = {
    val dc = Errors.trap("Can't dynamically load class _." << fqn) {
      Class.forName(fqn).newInstance.asInstanceOf[{
        val languageKey: String
        val nbEntries: Int
        val nbPluralForms: Int
        val pluralForms: String
        val generatedAt: String
        val javaVersion: String
        def computePluralForm(n: Int): Int
        def gettext(msgid: String): String
        def ngettext(msgid: String, n: Int): String
      }]
    }
    if (dc.languageKey != i18nLanguageKey.string)
      Errors.fatal("Invalid language key _ for class _ ; _ was expected." << (dc.languageKey, fqn, i18nLanguageKey))
    dc
  }

  def gettext(key: String): String = {
    val translation = dynamicClass.gettext(key)
    if (translation == null)
      if (_parent == null)
        null
      else
        _parent.gettext(key)
    else
      translation
  }

  def ngettext(key: String, n: Int): String = {
    val translation = dynamicClass.ngettext(key, n)
    if (translation == null)
      if (_parent == null)
        null
      else
        _parent.ngettext(key, n)
    else
      translation
  }
}

