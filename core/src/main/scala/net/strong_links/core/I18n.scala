package net.strong_links.core

import java.util.Locale
import scala.collection.mutable.{ SynchronizedMap, HashMap }
import java.util.IdentityHashMap

class I18nLocalization(val packageName: String, val language: String, val country: Option[String], val parent: Option[I18nLocalization]) {

  def usePluralRulePoString = "nplurals=???; plural=???"

  override def toString: String = "[Language: _, country: _, parent: _]" << (language, country, parent)

  val languageKey = I18nUtil.toLanguageKey(language, country)

  val className = "I18n_" + languageKey + "_" + packageName

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
    if (dc.languageKey != languageKey)
      Errors.fatal("Invalid language key _ for class _ ; _ was expected." << (dc.languageKey, className, languageKey))
    dc
  }

  def dummyGet = gettext("\uFFFF")

  def gettext(key: String): String = {
    val translation = dynamicClass.gettext(key)
    if (translation eq "")
      parent match {
        case None => ""
        case Some(p) => p.gettext(key)
      }
    else
      translation
  }

  def ngettext(key: String, n: Int): String = {
    val translation = dynamicClass.ngettext(key, n)
    if (translation eq "")
      parent match {
        case None => ""
        case Some(p) => p.ngettext(key, n)
      }
    else
      translation
  }
}

class I18nCodeLocalization(packageName: String, language: String, val usePluralRule: (Int) => Boolean, override val usePluralRulePoString: String)
  extends I18nLocalization(packageName, language, None, None)

object I18nCodeLocalization {
  def english(packageName: String) = new I18nCodeLocalization(packageName, "en", _ != 1, "nplurals=2; plural=(n == 1) ? 0 : 1")
  def french(packageName: String) = new I18nCodeLocalization(packageName, "fr", _ > 1, "nplurals=2; plural=(n <= 1) ? 0 : 1")
  def apply(packageName: String, languageCode: String) = languageCode match {
    case "en" => english(packageName)
    case "fr" => french(packageName)
    case _ => Errors.fatal("No code localization method for language _." << languageCode)
  }
}

class I18nCatalog(packageName: String, codeLocalization: I18nCodeLocalization, localizations: List[I18nLocalization]) {

  val codeUsePlural = codeLocalization.usePluralRule
  val codeLanguageKey = codeLocalization.languageKey

  I18nUtil.checkUniqueness(codeLocalization, localizations)

  // Try to get any string to force the dynamic class loading.
  localizations.foreach(_.dummyGet)

  private[core] val map = localizations.map(L => (L.languageKey, L)).toMap
}

protected class I18n(val catalog: I18nCatalog, val msgCtxt: Option[String], val msgid: String,
  val msgidPlural: Option[String], val n: Int) {

  def toString(languageKey: String): String = {

    def default = msgidPlural match { case None => msgid; case Some(p) => if (catalog.codeUsePlural(n)) p else msgid }

    if (languageKey eq catalog.codeLanguageKey)
      default
    else catalog.map.get(languageKey) match {
      case None =>
        default
      case Some(i18nLocalization) =>
        val searchedMsgid = msgCtxt match {
          case None => msgid
          case Some(ctx) => (ctx + "\u0000" + msgid).intern
        }
        val translation = msgidPlural match {
          case None => i18nLocalization.gettext(searchedMsgid)
          case _ => i18nLocalization.ngettext(searchedMsgid, n)
        }
        if (translation == "")
          default
        else
          translation
    }
  }

  override def toString: String = {
    val ulk = userLanguageKey.unsafeGet
    if (ulk == null) toString(I18nUtil.systemLanguageKey) else toString(ulk)
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

object I18nUtil {

  def toLanguageKey(language: String, country: Option[String]): String = {
    country match {
      case None => language
      case Some(null) => language
      case Some("") => language
      case Some(other) => language + "_" + other
    }
  }

  def toLanguageKey(locale: Locale): String = toLanguageKey(locale.getLanguage, Some(locale.getCountry))

  // System language key at startup. This is *not* expected to change in a server environment.
  val systemLanguageKey = {
    val locale = Locale.getDefault
    toLanguageKey(locale.getLanguage, Some(locale.getCountry))
  }

  def checkUniqueness(codeLocalization: I18nCodeLocalization, localizations: List[I18nLocalization]) {
    // Ensure that each language variation is handled only once.
    (codeLocalization :: localizations).groupBy(_.languageKey).toList.find(_._2.length > 1).map(_._1) match {
      case Some(culprit) => Errors.fatal("The localization with a language key_ exists more than once." << culprit)
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
