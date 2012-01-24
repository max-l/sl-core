package net.strong_links.core

import java.util.Locale
import java.io.File
import scala.collection.mutable.{ SynchronizedMap, HashMap }
//import java.util.IdentityHashMap

object I18nConfig {
  def validatePackageNameSegment(s: String) = {
    def invalid(why: LoggingParameter) {
      Errors.fatal("Invalid package name segment _" << s, why)
    }
    def check(ch: Char, digitsAllowed: Boolean) {
      val ok = (ch >= 'A') && (ch <= 'Z') || (ch >= 'a') && (ch <= 'z') || (ch == '_') ||
        digitsAllowed && (ch >= '0') && (ch <= '9')
      if (!ok)
        invalid("Invalid character _." << ch)
    }
    if (s == "")
      invalid("Empty segment")
    check(s.head, false)
    s.tail.foreach(check(_, true))
  }

  def checkPackageSegments(segments: List[String]) {
    if (segments.isEmpty)
      Errors.fatal("No segments found in package name.")
    segments.foreach(validatePackageNameSegment)
  }
}

class I18nConfig(val packageName: String, languageKey: String, localizationsStr: String) {

  val codeLocalization = I18nCodeLocalization(packageName, I18nLanguageKey.from(languageKey).string)

  val (masterLocalizations, subLocalizations) = {

    class Info(val i18nLanguageKey: I18nLanguageKey, val parentI18nLanguageKey: Option[I18nLanguageKey])

    val infos = Util.split(localizationsStr, ",").map(_.trim).filter(!_.isEmpty).map { s =>
      Errors.trap("Invalid language/variant specification _" << s) {
        val (i18nLanguageKey, parentI18nLanguageKey) = if (s.contains(':')) {
          val (lk, plk) = Util.splitTwo(s, ':')
          (I18nLanguageKey.from(lk), Some(I18nLanguageKey.from(plk)))
        } else
          (I18nLanguageKey.from(s), None)
        new Info(i18nLanguageKey, parentI18nLanguageKey)
      }
    }

    val (masterInfos, subInfos) = infos.partition(_.parentI18nLanguageKey == None)

    val ml = masterInfos.map(m => new I18nLocalization(packageName, m.i18nLanguageKey, None)).sorted

    val sl = {
      val allMasterLocalizations = codeLocalization :: ml
      subInfos.map(s =>
        s.parentI18nLanguageKey match {
          case Some(plk) =>
            allMasterLocalizations.filter(_.i18nLanguageKey == plk) match {
              case Nil => Errors.fatal("Master localization _ not found for sublocalization _." << (plk, s.i18nLanguageKey))
              case List(m) => new I18nLocalization(packageName, s.i18nLanguageKey, Some(m))
              case list => Errors.fatal("Master localization _ found _ times." << (plk, list.length))
            }
          case None => Errors.fatal("No parent for sub-localization.")
        }).sorted
    }
    (ml, sl)
  }

  val allLocalizations = masterLocalizations ::: subLocalizations

  I18nUtil.checkUniqueness(codeLocalization, allLocalizations)

  val packageSegments = Util.split(packageName, '.')
  I18nConfig.checkPackageSegments(packageSegments)

  def toCatalog = new I18nCatalog(packageName, codeLocalization, allLocalizations)
}

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

  def className = i18nLanguageKey + "_" + packageName.replace(".", "_")

  private lazy val dynamicClass = {
    val fqn = packageName + "." + className
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

  def fileFor(dir: File, extension: String) =
    new File(dir.path + IO.dirSeparator + className + "." + extension)
}

class I18nCodeLocalization(packageName: String, i18nLanguageKey: I18nLanguageKey, val usePluralRule: (Int) => Boolean, val usePluralRulePoString: String)
  extends I18nLocalization(packageName, i18nLanguageKey, None)

object I18nCodeLocalization {

  // Thanks to people behind "http://translate.sourceforge.net/wiki/l10n/pluralforms"
  // Acholi
  def ach(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ach", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Afrikaans
  def af(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("af", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Akan
  def ak(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ak", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Amharic
  def am(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("am", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Aragonese
  def an(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("an", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Mapudungun
  def arn(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("arn", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Asturian
  def ast(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ast", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Aymará
  def ay(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ay", None), n => false, "nplurals=1; plural=0")
  // Azerbaijani
  def az(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("az", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Bulgarian
  def bg(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("bg", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Bengali
  def bn(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("bn", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Tibetan
  def bo(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("bo", None), n => false, "nplurals=1; plural=0")
  // Breton
  def br(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("br", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Catalan
  def ca(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ca", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Chiga
  def cgg(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("cgg", None), n => false, "nplurals=1; plural=0")
  // Danish
  def da(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("da", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // German
  def de(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("de", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Dzongkha
  def dz(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("dz", None), n => false, "nplurals=1; plural=0")
  // Greek
  def el(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("el", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // English
  def en(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("en", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Esperanto
  def eo(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("eo", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Spanish
  def es(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("es", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Estonian
  def et(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("et", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Basque
  def eu(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("eu", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Persian
  def fa(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("fa", None), n => false, "nplurals=1; plural=0")
  // Finnish
  def fi(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("fi", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Filipino
  def fil(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("fil", None), n => n > 1, "nplurals=2; plural=n > 1")
  // Faroese
  def fo(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("fo", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // French
  def fr(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("fr", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Friulian
  def fur(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("fur", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Frisian
  def fy(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("fy", None), n => (n != 1), "nplurals=2; plural=(n != 1) ")
  // Galician
  def gl(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("gl", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Gujarati
  def gu(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("gu", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Gun
  def gun(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("gun", None), n => (n > 1), "nplurals=2; plural = (n > 1)")
  // Hausa
  def ha(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ha", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Hebrew
  def he(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("he", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Hindi
  def hi(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("hi", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Armenian
  def hy(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("hy", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Hungarian
  def hu(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("hu", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Interlingua
  def ia(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ia", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Indonesian
  def id(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("id", None), n => false, "nplurals=1; plural=0")
  // Icelandic
  def is(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("is", None), n => (n % 10 != 1 || n % 100 == 11), "nplurals=2; plural=(n % 10 != 1 || n % 100== 11)")
  // Italian
  def it(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("it", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Japanese
  def ja(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ja", None), n => false, "nplurals=1; plural=0")
  // Lojban
  def jbo(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("jbo", None), n => false, "nplurals=1; plural=0")
  // Javanese
  def jv(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("jv", None), n => (n != 0), "nplurals=2; plural=(n != 0)")
  // Georgian
  def ka(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ka", None), n => false, "nplurals=1; plural=0")
  // Kazakh
  def kk(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("kk", None), n => false, "nplurals=1; plural=0")
  // Khmer
  def km(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("km", None), n => false, "nplurals=1; plural=0")
  // Kannada
  def kn(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("kn", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Korean
  def ko(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ko", None), n => false, "nplurals=1; plural=0")
  // Kurdish
  def ku(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ku", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Kyrgyz
  def ky(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ky", None), n => false, "nplurals=1; plural=0")
  // Letzeburgesch
  def lb(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("lb", None), n => (n != 1), "nplurals=2; plural=(n != 1) ")
  // Lingala
  def ln(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ln", None), n => n > 1, "nplurals=2; plural=n > 1;")
  // Lao
  def lo(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("lo", None), n => false, "nplurals=1; plural=0")
  // Maithili
  def mai(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("mai", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Mauritian Creole
  def mfe(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("mfe", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Malagasy
  def mg(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("mg", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Maori
  def mi(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("mi", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Malayalam
  def ml(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ml", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Mongolian
  def mn(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("mn", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Marathi
  def mr(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("mr", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Malay
  def ms(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ms", None), n => false, "nplurals=1; plural=0")
  // Nahuatl
  def nah(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("nah", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Neapolitan
  def nap(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("nap", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Norwegian Bokmal
  def nb(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("nb", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Nepali
  def ne(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ne", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Dutch
  def nl(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("nl", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Northern Sami
  def se(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("se", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Norwegian Nynorsk
  def nn(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("nn", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Norwegian (old code)
  def no(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("no", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Northern Sotho
  def nso(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("nso", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Occitan
  def oc(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("oc", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Oriya
  def or(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("or", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Pashto
  def ps(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ps", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Punjabi
  def pa(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("pa", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Papiamento
  def pap(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("pap", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Piemontese
  def pms(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("pms", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Portuguese
  def pt(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("pt", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Brazilian Portuguese (BR)
  def pt_BR(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("pt", Some("BR")), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Romansh
  def rm(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("rm", None), n => (n != 1), "nplurals=2; plural=(n != 1);")
  // Yakut
  def sah(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("sah", None), n => false, "nplurals=1; plural=0 ")
  // Scots
  def sco(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("sco", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Sinhala
  def si(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("si", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Somali
  def so(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("so", None), n => n != 1, "nplurals=2; plural=n != 1")
  // Songhay
  def son(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("son", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Albanian
  def sq(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("sq", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Sundanese
  def su(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("su", None), n => false, "nplurals=1; plural=0")
  // Swahili
  def sw(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("sw", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Swedish
  def sv(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("sv", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Tamil
  def ta(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ta", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Telugu
  def te(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("te", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Tajik
  def tg(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("tg", None), n => false, "nplurals=1; plural=0")
  // Tigrinya
  def ti(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ti", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Thai
  def th(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("th", None), n => false, "nplurals=1; plural=0")
  // Turkmen
  def tk(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("tk", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Turkish
  def tr(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("tr", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Tatar
  def tt(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("tt", None), n => false, "nplurals=1; plural=0")
  // Uyghur
  def ug(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ug", None), n => false, "nplurals=1; plural=0;")
  // Urdu
  def ur(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("ur", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Uzbek
  def uz(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("uz", None), n => false, "nplurals=1; plural=0;")
  // Vietnamese
  def vi(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("vi", None), n => false, "nplurals=1; plural=0")
  // Walloon
  def wa(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("wa", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Wolof
  def wo(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("wo", None), n => false, "nplurals=1; plural=0")
  // Yoruba
  def yo(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("yo", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Chinese
  def zh(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("zh", None), n => false, "nplurals=1; plural=0")
  // Chinese (rare)
  def zh2(packageName: String) = new I18nCodeLocalization(packageName, I18nLanguageKey("zh2", None), n => (n > 1), "nplurals=2; plural=(n > 1)")

  private lazy val codeLocalizationMethods = getClass.getMethods.toList.filter(m => {
    val params = m.getParameterTypes;
    m.getReturnType == classOf[I18nCodeLocalization] &&
      params.length == 1 &&
      params(0) == classOf[String]
  })

  def apply(packageName: String, languageKey: String) = {
    codeLocalizationMethods.map(m => m.invoke(this, Seq(packageName): _*).asInstanceOf[I18nCodeLocalization])
      .filter(_.i18nLanguageKey.string == languageKey) match {
        case List(i18nCodeLocalization) => i18nCodeLocalization
        case list => Errors.fatal("_ localization methods found for language key _." << (list.length, languageKey))
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

    def default = {
      val x = if (n == Int.MaxValue) msgid else if (catalog.codeUsePlural(n)) msgidPlural else msgid
      if (x == null)
        Errors.fatal("Default translation failed on _, _, _." << (msgCtxt, msgid, msgidPlural))
      x
    }

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

  def validate(os: Option[String], what: String) = {
    os match {
      case None =>
      case Some(s) =>
        if (s.trim != s)
          Errors.fatal("The _ string _ has leading or trailing whitespace." << (what, s))
        if (s == "")
          Errors.fatal("Empty _ string." << what)
    }
  }

  def compute(msgCtxt: Option[String], msgid: String) = msgCtxt match {
    case None => msgid
    case Some(ctx) => ctx + "\u0000" + msgid
  }

  def computeForCompiler(msgCtxt: Option[String], msgid: String) = msgCtxt match {
    case None => msgid
    case Some(ctx) => ctx + "\\u0000" + msgid
  }

  def computeForHuman(msgCtxt: Option[String], msgid: String): String = msgCtxt match {
    case None => "msgid _" <<< msgid
    case Some(ctx) => "msgctxt _ and msgid _" <<< (ctx, msgid)
  }
}

abstract class GeneralString

class StringGeneralString(s: String) extends GeneralString {
  override def toString = s
}

class I18nGeneralString(i18n: I18n) extends GeneralString {
  override def toString = i18n.toString
}
