package net.strong_links.core

import java.util.Locale
import java.io.File
import scala.collection.mutable.{ SynchronizedMap, HashMap }
//import java.util.IdentityHashMap

object I18nLanguageKey {

  def from(language: String, country: Option[String]): I18nLanguageKey = {
    def normalize(s: String) = if (s == null) "" else s.trim
    def normalizedLanguage = normalize(language).toLowerCase
    if (normalizedLanguage == "")
      Errors.fatal("No language found.")
    val normalizedCountry = country match {
      case None => None
      case Some(c) => normalize(c).toUpperCase match {
        case "" => Errors.fatal("No country found.")
        case x => Some(x)
      }
    }
    new I18nLanguageKey(normalizedLanguage, normalizedCountry)
  }

  def from(locale: Locale): I18nLanguageKey = Errors.trap("Invalid locale _" << locale) {
    from(locale.getLanguage, locale.getCountry match { case null => None; case "" => None; case x => Some(x) })
  }

  def from(languageKey: String): I18nLanguageKey = Errors.trap("Invalid language key _" << languageKey) {
    if (languageKey.contains('_')) {
      val (a, b) = Util.splitTwo(languageKey, '_')
      from(a, Some(b))
    } else
      from(languageKey, None)
  }

  // System language key at startup. This is *not* expected to change in a server environment.
  lazy val system = from(Locale.getDefault)
}

case class I18nLanguageKey(language: String, country: Option[String]) {

  // Precompute its string representation, which will exist only once in the server.
  val string = (country match {
    case None => language
    case Some(c) => language + "_" + c
  }).intern

  override def toString = string
}

class I18nLocalization(val packageName: String, val i18nLanguageKey: I18nLanguageKey, val parent: Option[I18nLocalization]) {

  // Keep an internal less safe reference to the parent as this will be faster at run-time.
  private val _parent = parent match { case None => null; case Some(p) => p }

  override def toString = parent match { case None => i18nLanguageKey.string; case Some(p) => i18nLanguageKey + ":" + p.i18nLanguageKey }

  private def className = "I18n_" + i18nLanguageKey + "_" + packageName

  private lazy val dynamicClass = {
    val dc = Errors.trap("Can't dynamically load class _." << className) {
      Class.forName(className).newInstance.asInstanceOf[{
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
      Errors.fatal("Invalid language key _ for class _ ; _ was expected." << (dc.languageKey, className, i18nLanguageKey))
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

  def fileFor(dir: File, extension: String) =
    new File(dir.path + IO.dirSeparator + className + "." + extension)
}

class I18nCodeLocalization(packageName: String, i18nLanguageKey: I18nLanguageKey, val usePluralRule: (Int) => Boolean, val usePluralRulePoString: String)
  extends I18nLocalization(packageName, i18nLanguageKey, None)

object I18nCodeLocalization {

  // Thanks to people behind "http://translate.sourceforge.net/wiki/l10n/pluralforms"
  def en(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("en", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  def fr(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("fr", None), n => (n > 1), "nplurals=2; plural=(n > 1)")

  private lazy val codeLocalizationMethods = getClass.getMethods.toList.filter(m => {
    val params = m.getParameterTypes;
    m.getReturnType == classOf[I18nCodeLocalization] &&
      params.length == 1 &&
      params(0) == classOf[String]
  })

  def apply(packageName: String, i18nLanguageKey: I18nLanguageKey) = {
    codeLocalizationMethods.map(m => m.invoke(this, Seq(packageName): _*).asInstanceOf[I18nCodeLocalization])
      .filter(_.i18nLanguageKey.string == i18nLanguageKey.string) match {
        case List(i18nCodeLocalization) => i18nCodeLocalization
        case list => Errors.fatal("_ localization methods found for language key _." << (list.length, i18nLanguageKey))
      }
  }
}

class I18nCatalog(packageName: String, codeLocalization: I18nCodeLocalization, localizations: List[I18nLocalization]) {

  val codeUsePlural = codeLocalization.usePluralRule
  val codeI18nLanguageKey = codeLocalization.i18nLanguageKey

  // Make sure each localization appears only once.
  I18nUtil.checkUniqueness(codeLocalization, localizations)

  // Try to get any string to force the dynamic class loading.
  localizations.foreach(_.gettext("\uFFFF"))

  // Create a map to find a localization according to its language key represented (as a single string).
  private[core] val map = localizations.map(L => (L.i18nLanguageKey.toString, L)).toMap
}

protected class I18n(catalog: I18nCatalog, msgCtxt: String, msgid: String, msgidPlural: String, n: Int) {

  lazy val key = if (msgCtxt == null) msgid else (msgCtxt + "\u0000" + msgid).intern

  def toString(i18nLanguageKey: I18nLanguageKey): String = {

    def default = if (n == Int.MaxValue) msgid else if (catalog.codeUsePlural(n)) msgidPlural else msgid

    if (i18nLanguageKey.string eq catalog.codeI18nLanguageKey.string)
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

object I18nUtil {

  // Ensure that each language variation is handled only once.
  def checkUniqueness(codeLocalization: I18nCodeLocalization, localizations: List[I18nLocalization]) {
    (codeLocalization :: localizations).groupBy(_.i18nLanguageKey).toList.find(_._2.length > 1).map(_._1) match {
      case Some(culprit) => Errors.fatal("The localization with a language key _ exists more than once." << culprit)
      case None =>
    }
  }
}

abstract class GeneralString

class StringGeneralString(s: String) extends GeneralString {
  override def toString = s
}

class I18nGeneralString(i18n: I18n) extends GeneralString {
  override def toString = i18n.toString
}
