package net.strong_links.core

import java.util.Locale
import scala.collection.mutable.{ SynchronizedMap, HashMap }
import java.util.IdentityHashMap

object I18nUtil {
  def makeLanguageKey(language: String, country: String): String = {
    country match {
      case "" => language.toLowerCase
      case _ => language.toLowerCase + "_" + country.toUpperCase
    }
  }

  def makeLanguageKey(locale: Locale): String = {
    makeLanguageKey(locale.getLanguage, locale.getCountry)
  }

  def classNameFor(packageName: String, languageKey: String) = {
    Util.split(packageName, ".").map(_.capitalize).mkString + "_" + languageKey
  }

  private class localizationInfo(pLanguage: String, pCountry: String, pParentLanguage: String, val originalData: String)
    extends Ordered[localizationInfo] {
    val language = pLanguage.trim.toLowerCase
    val country = pCountry.trim.toUpperCase
    val parentLanguage = pParentLanguage.trim.toLowerCase
    if (language.length != 2)
      Errors.fatal("Invalid language code _ in localization _." << (pLanguage, originalData))
    if (!(country.isEmpty || (country.length == 2)))
      Errors.fatal("Invalid country code _ in localization _." << (pCountry, originalData))
    if (!(parentLanguage.isEmpty || (parentLanguage.length == 2)))
      Errors.fatal("Invalid parent language code _ in localization _." << (pParentLanguage, originalData))
    def key = country match { case "" => language; case c => language + "_" + c }
    def compare(that: localizationInfo) = this.key compare that.key
    override def toString = originalData
  }

  def makeCodeLocalizationsFrom(codeLocalizationLanguage: String) = {
    val x = new localizationInfo(codeLocalizationLanguage, "", "", "<Package code localization>")
    new I18nCodeLocalization(x.language, n => false)
  }

  def makeLocalizationsFrom(codeLocalization: I18nCodeLocalization, p: String) = {
    val partialList =
      Util.split(p, ",").map(_.trim).filter(!_.isEmpty).map { s =>
        Errors.trap("Invalid language/variant specification _" << s) {
          val (id, parentLanguage) = if (s.contains(":")) Util.splitTwo(s, ':') else (s, "")
          val (language, country) = if (id.contains("_")) Util.splitTwo(id, '_') else (id, "")
          new localizationInfo(language, country, parentLanguage, s)
        }
      }
    val fullList = new localizationInfo(codeLocalization.language, "", "", "") +: partialList
    val list = fullList.sortWith(_ < _)
    list.groupBy(_.key).filter(_._2.length > 1).map(_._1) match {
      case Nil =>
      case weirdos => Errors.fatal("Language/country combination _ appears more than once." << weirdos.head)
    }
    val (masterLocalizationsSet, subLocalizationsSet) = list.partition(_.parentLanguage == "")
    val masterLocalizations = masterLocalizationsSet.map(e => I18nLocalization(e.language, e.country))
    val subLocalizations = subLocalizationsSet.map { e =>
      masterLocalizations.find(_.language == e.language) match {
        case None => Errors.fatal("Master localization _ not found for localization _." << (e.language, e))
        case Some(parent) => I18nLocalization(e.language, e.country, parent)
      }
    }
    masterLocalizations.filter(_.language != codeLocalization.language) ::: subLocalizations
  }
}

object I18nLocalization {
  def apply(language: String) =
    new I18nLocalization(language, "", None)
  def apply(language: String, country: String) =
    new I18nLocalization(language, country, None)
  def apply(language: String, country: String, parent: I18nLocalization) =
    new I18nLocalization(language, country, Some(parent))
}

class I18nLocalization(val language: String, val country: String, val parent: Option[I18nLocalization]) {
  def this(language: String, country: String, parent: I18nLocalization) = this(language, country, Some(parent))
  def this(language: String, country: String) = this(language, country, None)
  def this(language: String) = this(language, "", None)
  def locale = I18nLocale(language, country)
  val optionCountry = if (country == "") None else Some(country)
  override def toString: String = "[Language _, country _, parent _]" << (language, optionCountry, parent)
}

class I18nCodeLocalization(override val language: String, val usePluralRule: (Int) => Boolean)
  extends I18nLocalization(language)

object I18nEnglishCodeLocalization extends I18nCodeLocalization("en", _ != 1)
object I18nFrenchCodeLocalization extends I18nCodeLocalization("fr", _ > 1)

/**
 * This object caches the various Locales. Locales must exist only *once* in the system
 * because they are compared on their reference for speed. Thus, the only way to get
 * a locale is by calling the apply method of this object.
 */
object I18nLocale {
  private val cache = new HashMap[String, Locale] with SynchronizedMap[String, Locale]

  def notNull(s: String) = if (s == null) "" else s

  def apply(language: String, country: String = ""): Locale = {
    val languageKey = I18nUtil.makeLanguageKey(language, country)
    if (!cache.contains(languageKey))
      cache += (languageKey -> new Locale(language, country))
    cache(languageKey)
  }

  def apply(locale: Locale): Locale = {
    apply(notNull(locale.getLanguage), notNull(locale.getCountry))
  }

  def getLocaleFor(language: String, country: String = ""): Locale = {
    val languageKey = I18nUtil.makeLanguageKey(language, country)
    cache.get(languageKey) match {
      case None => Errors.fatal("No locale found for language key _." << languageKey)
      case Some(locale) => locale
    }
  }

  def getLocaleFor(locale: Locale): Locale = {
    getLocaleFor(notNull(locale.getLanguage), notNull(locale.getCountry))
  }

  def checkLocale(locale: Locale) = {
    if (getLocaleFor(locale.getLanguage, locale.getCountry) != locale)
      Errors.fatal("Locale has not been generated by I18nLocales.")
  }
}

class I18nGettextClass(packageName: String, locale: Locale, parentClass: Option[I18nGettextClass]) {
  private val languageKey = I18nUtil.makeLanguageKey(locale)
  private val dynamicClassName = I18nUtil.classNameFor(packageName, languageKey)
  private val dynamicClass = loadIt

  private def loadIt = {
    val dc = Errors.trap("Can't dynamically load class _." << dynamicClassName) {
      Class.forName(dynamicClassName).newInstance.asInstanceOf[{
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
    if (dc.languageKey != languageKey)
      Errors.fatal("Invalid language key _ for class _ ; _ was expected." << (dc.languageKey, dynamicClassName, languageKey))
    dc
  }

  def gettext(key: String): String = {
    val translation = dynamicClass.gettext(key)
    if (translation == "")
      parentClass match {
        case None => ""
        case Some(pc) => pc.gettext(key)
      }
    else
      translation
  }

  def ngettext(key: String, n: Int): String = {
    val translation = dynamicClass.ngettext(key, n)
    if (translation == "")
      parentClass match {
        case None => ""
        case Some(pc) => pc.ngettext(key, n)
      }
    else
      translation
  }
}

class I18nCatalog(packageName: String, codeLocalization: I18nCodeLocalization, localizations: List[I18nLocalization]) {

  // Keep references on the package code information.
  val codeLocale = codeLocalization.locale
  val usePlural = codeLocalization.usePluralRule

  // Fast map for loaded classes.
  private val loadedClasses = new IdentityHashMap[Locale, I18nGettextClass]()

  // Make sure no localization is the same as the package code localization.
  localizations.foreach { localization =>
    if (localization.locale == codeLocalization.locale)
      Errors.fatal("The localization _ has the same locale as the package code localization." << localization)
  }

  // Partition localizations into masters and subs.
  val (masters, subs) = localizations.partition(_.parent == None)

  // Load master translation classes.
  masters.foreach(m => loadTranslationClass(m.locale))

  // Load sub translation classes.
  subs.foreach { s =>
    val parent = s.parent.get
    if (!loadedClasses.containsKey(parent.locale))
      Errors.fatal("Sub-localization _ has a parent not supplied in the list." << s)
    loadTranslationClass(s.locale, Some(translationClassFor(parent.locale)))
  }

  private def loadTranslationClass(locale: Locale, parentClass: Option[I18nGettextClass] = None) = {
    // Make sure we know this locale
    I18nLocale.checkLocale(locale)
    if (loadedClasses.containsKey(locale))
      Errors.fatal("Translation class already loaded for package _ and language key _." << (packageName, I18nUtil.makeLanguageKey(locale)))
    val gettextClass = new I18nGettextClass(packageName, locale, parentClass)
    loadedClasses.put(locale, gettextClass)
    gettextClass
  }

  def translationClassFor(locale: Locale) = {
    val result = loadedClasses.get(locale)
    if (result == null)
      Errors.fatal("Translation class not loaded for package _ and language key _." << (packageName, I18nUtil.makeLanguageKey(locale)))
    result
  }
}

protected class I18n(val catalog: I18nCatalog, val msgCtxt: Option[String], val msgid: String,
  val msgidPlural: Option[String], val n: Int) {

  def toString(locale: Locale): String = {

    def default = msgidPlural match { case None => msgid; case Some(p) => if (catalog.usePlural(n)) p else msgid }

    if (locale eq catalog.codeLocale)
      default
    else {
      val tc = catalog.translationClassFor(locale)
      val searchedMsgid = msgCtxt match {
        case None => msgid
        case Some(ctx) => (ctx + "\u0000" + msgid).intern
      }
      val translation = msgidPlural match {
        case None => tc.gettext(searchedMsgid)
        case _ => tc.ngettext(searchedMsgid, n)
      }
      if (translation == "")
        default
      else
        translation
    }
  }

  override def toString: String = {
    var locale = userLocale.unsafeGet
    if (locale == null)
      locale = I18n.systemLocale
    toString(locale)
  }

  def <<(args: Any*) = {
    new PluggedI18n(this, Some(args), false)
  }

  def <<<(args: Any*) = {
    new PluggedI18n(this, Some(args), true)
  }
}

class PluggedI18n(from: I18n, args: Option[Seq[Any]], quotedDefault: Boolean)
  extends I18n(from.catalog, from.msgCtxt, from.msgid, from.msgidPlural, from.n) {

  override def toString: String = {
    toString(false, quotedDefault)
  }

  def toString(failsafe: Boolean, quoted: Boolean): String = {
    PluggedArguments.format(super.toString, args, failsafe, quoted)
  }
}

object I18n {

  // System locale at startup. This is *not* expected to change in a server environment.
  val systemLocale = I18nLocale(Locale.getDefault)

  def apply(msgid: String)(implicit catalog: I18nCatalog) =
    new I18n(catalog, None, msgid, None, 0)
}

object I18nPlural {
  def apply(msgid: String, msgidPlural: String, n: Int)(implicit catalog: I18nCatalog) =
    new I18n(catalog, None, msgid, Some(msgidPlural), n)
}

object I18nCtxt {
  def apply(msgCtxt: String, msgid: String)(implicit catalog: I18nCatalog) =
    new I18n(catalog, Some(msgCtxt), msgid, None, 0)
}

object I18nPluralCtxt {
  def apply(msgCtxt: String, msgid: String, msgPlural: String, n: Int)(implicit catalog: I18nCatalog) =
    new I18n(catalog, Some(msgCtxt), msgid, Some(msgPlural), n)
}

abstract class GeneralString

class StringGeneralString(s: String) extends GeneralString {
  override def toString = s
}

class I18nGeneralString(i18n: I18n) extends GeneralString {
  override def toString = i18n.toString
}
