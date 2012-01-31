package com.strong_links.core

class I18nLocalization(val packageName: String, val packageNameSegments: List[String],
  val i18nConfigLocalization: I18nConfigLocalization, val parent: Option[I18nLocalization]) {

  val className = i18nConfigLocalization.classNameFor(packageNameSegments)

  val fqn = (packageNameSegments :+ className).mkString(".")

  // Keep an internal less safe reference to the parent as this will be faster at run-time.
  private val _parent = parent match { case None => null; case Some(p) => p }

  private val dynamicClass = Errors.trap("Can't dynamically load class _." << fqn) {
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

  if (dynamicClass.languageKey != i18nConfigLocalization.i18nLocale.key)
    Errors.fatal("Invalid localization _ for class _ ; _ was expected." << (dynamicClass.languageKey, fqn, i18nConfigLocalization.i18nLocale.key))

  override def toString = i18nConfigLocalization.toString

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

