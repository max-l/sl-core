package net.strong_links.core

import java.util.Locale

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

  def toPackageSegments(packageName: String) = {
    val s = Util.split(packageName, '.')
    checkPackageSegments(s)
    s
  }
}

class I18nConfig(val packageName: String, codeLanguageKey: String, localizationsStr: String) {

  def this(packageName: String, codeLanguageKey: String) = this(packageName, codeLanguageKey, "")

  val packageSegments = I18nConfig.toPackageSegments(packageName)

  val codeLocalization = I18nCodeLocalization.search(packageSegments, I18nLanguageKey.from(codeLanguageKey).string)

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

    val ml = masterInfos.map(m => new I18nLocalization(packageSegments, m.i18nLanguageKey, None)).sorted

    val sl = {
      val allMasterLocalizations = codeLocalization :: ml
      subInfos.map(s =>
        s.parentI18nLanguageKey match {
          case Some(plk) =>
            allMasterLocalizations.filter(_.i18nLanguageKey == plk) match {
              case Nil => Errors.fatal("Master localization _ not found for sublocalization _." << (plk, s.i18nLanguageKey))
              case List(m) => new I18nLocalization(packageSegments, s.i18nLanguageKey, Some(m))
              case list => Errors.fatal("Master localization _ found _ times." << (plk, list.length))
            }
          case None => Errors.fatal("No parent for sub-localization.")
        }).sorted
    }
    (ml, sl)
  }

  val allLocalizations = masterLocalizations ::: subLocalizations

  I18nUtil.checkUniqueness(codeLocalization, allLocalizations)

  def toCatalog = new I18nCatalog(packageSegments, codeLocalization, allLocalizations)
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

class I18nCodeLocalization(packageSegments: List[String], i18nLanguageKey: I18nLanguageKey, val usePluralRule: (Int) => Boolean, val usePluralRulePoString: String)
  extends I18nLocalization(packageSegments, i18nLanguageKey, None)

object I18nCodeLocalization {

  // Thanks to people behind "http://translate.sourceforge.net/wiki/l10n/pluralforms"
  // Acholi
  def ach(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ach", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Afrikaans
  def af(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("af", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Akan
  def ak(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ak", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Amharic
  def am(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("am", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Aragonese
  def an(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("an", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Mapudungun
  def arn(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("arn", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Asturian
  def ast(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ast", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Aymará
  def ay(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ay", None), n => false, "nplurals=1; plural=0")
  // Azerbaijani
  def az(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("az", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Bulgarian
  def bg(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("bg", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Bengali
  def bn(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("bn", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Tibetan
  def bo(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("bo", None), n => false, "nplurals=1; plural=0")
  // Breton
  def br(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("br", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Catalan
  def ca(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ca", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Chiga
  def cgg(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("cgg", None), n => false, "nplurals=1; plural=0")
  // Danish
  def da(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("da", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // German
  def de(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("de", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Dzongkha
  def dz(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("dz", None), n => false, "nplurals=1; plural=0")
  // Greek
  def el(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("el", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // English
  def en(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("en", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Esperanto
  def eo(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("eo", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Spanish
  def es(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("es", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Estonian
  def et(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("et", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Basque
  def eu(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("eu", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Persian
  def fa(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("fa", None), n => false, "nplurals=1; plural=0")
  // Finnish
  def fi(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("fi", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Filipino
  def fil(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("fil", None), n => n > 1, "nplurals=2; plural=n > 1")
  // Faroese
  def fo(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("fo", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // French
  def fr(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("fr", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Friulian
  def fur(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("fur", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Frisian
  def fy(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("fy", None), n => (n != 1), "nplurals=2; plural=(n != 1) ")
  // Galician
  def gl(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("gl", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Gujarati
  def gu(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("gu", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Gun
  def gun(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("gun", None), n => (n > 1), "nplurals=2; plural = (n > 1)")
  // Hausa
  def ha(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ha", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Hebrew
  def he(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("he", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Hindi
  def hi(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("hi", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Armenian
  def hy(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("hy", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Hungarian
  def hu(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("hu", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Interlingua
  def ia(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ia", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Indonesian
  def id(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("id", None), n => false, "nplurals=1; plural=0")
  // Icelandic
  def is(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("is", None), n => (n % 10 != 1 || n % 100 == 11), "nplurals=2; plural=(n % 10 != 1 || n % 100== 11)")
  // Italian
  def it(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("it", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Japanese
  def ja(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ja", None), n => false, "nplurals=1; plural=0")
  // Lojban
  def jbo(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("jbo", None), n => false, "nplurals=1; plural=0")
  // Javanese
  def jv(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("jv", None), n => (n != 0), "nplurals=2; plural=(n != 0)")
  // Georgian
  def ka(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ka", None), n => false, "nplurals=1; plural=0")
  // Kazakh
  def kk(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("kk", None), n => false, "nplurals=1; plural=0")
  // Khmer
  def km(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("km", None), n => false, "nplurals=1; plural=0")
  // Kannada
  def kn(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("kn", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Korean
  def ko(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ko", None), n => false, "nplurals=1; plural=0")
  // Kurdish
  def ku(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ku", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Kyrgyz
  def ky(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ky", None), n => false, "nplurals=1; plural=0")
  // Letzeburgesch
  def lb(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("lb", None), n => (n != 1), "nplurals=2; plural=(n != 1) ")
  // Lingala
  def ln(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ln", None), n => n > 1, "nplurals=2; plural=n > 1;")
  // Lao
  def lo(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("lo", None), n => false, "nplurals=1; plural=0")
  // Maithili
  def mai(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("mai", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Mauritian Creole
  def mfe(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("mfe", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Malagasy
  def mg(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("mg", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Maori
  def mi(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("mi", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Malayalam
  def ml(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ml", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Mongolian
  def mn(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("mn", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Marathi
  def mr(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("mr", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Malay
  def ms(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ms", None), n => false, "nplurals=1; plural=0")
  // Nahuatl
  def nah(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("nah", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Neapolitan
  def nap(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("nap", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Norwegian Bokmal
  def nb(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("nb", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Nepali
  def ne(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ne", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Dutch
  def nl(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("nl", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Northern Sami
  def se(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("se", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Norwegian Nynorsk
  def nn(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("nn", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Norwegian (old code)
  def no(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("no", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Northern Sotho
  def nso(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("nso", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Occitan
  def oc(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("oc", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Oriya
  def or(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("or", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Pashto
  def ps(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ps", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Punjabi
  def pa(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("pa", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Papiamento
  def pap(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("pap", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Piemontese
  def pms(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("pms", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Portuguese
  def pt(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("pt", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Brazilian Portuguese (BR)
  def pt_BR(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("pt", Some("BR")), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Romansh
  def rm(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("rm", None), n => (n != 1), "nplurals=2; plural=(n != 1);")
  // Yakut
  def sah(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("sah", None), n => false, "nplurals=1; plural=0 ")
  // Scots
  def sco(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("sco", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Sinhala
  def si(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("si", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Somali
  def so(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("so", None), n => n != 1, "nplurals=2; plural=n != 1")
  // Songhay
  def son(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("son", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Albanian
  def sq(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("sq", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Sundanese
  def su(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("su", None), n => false, "nplurals=1; plural=0")
  // Swahili
  def sw(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("sw", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Swedish
  def sv(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("sv", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Tamil
  def ta(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ta", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Telugu
  def te(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("te", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Tajik
  def tg(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("tg", None), n => false, "nplurals=1; plural=0")
  // Tigrinya
  def ti(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ti", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Thai
  def th(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("th", None), n => false, "nplurals=1; plural=0")
  // Turkmen
  def tk(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("tk", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Turkish
  def tr(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("tr", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Tatar
  def tt(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("tt", None), n => false, "nplurals=1; plural=0")
  // Uyghur
  def ug(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ug", None), n => false, "nplurals=1; plural=0;")
  // Urdu
  def ur(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("ur", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Uzbek
  def uz(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("uz", None), n => false, "nplurals=1; plural=0;")
  // Vietnamese
  def vi(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("vi", None), n => false, "nplurals=1; plural=0")
  // Walloon
  def wa(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("wa", None), n => (n > 1), "nplurals=2; plural=(n > 1)")
  // Wolof
  def wo(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("wo", None), n => false, "nplurals=1; plural=0")
  // Yoruba
  def yo(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("yo", None), n => (n != 1), "nplurals=2; plural=(n != 1)")
  // Chinese
  def zh(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("zh", None), n => false, "nplurals=1; plural=0")
  // Chinese (rare)
  def zh2(ps: List[String]) = new I18nCodeLocalization(ps, I18nLanguageKey("zh2", None), n => (n > 1), "nplurals=2; plural=(n > 1)")

  private lazy val codeLocalizationMethods = getClass.getMethods.toList.filter(m => {
    val params = m.getParameterTypes;
    m.getReturnType == classOf[I18nCodeLocalization] &&
      params.length == 1 &&
      params(0) == classOf[List[String]]
  })

  def search(packageSegments: List[String], languageKey: String) = {
    codeLocalizationMethods.map(m => m.invoke(this, Seq(packageSegments): _*).asInstanceOf[I18nCodeLocalization])
      .filter(_.i18nLanguageKey.string == languageKey) match {
        case List(i18nCodeLocalization) => i18nCodeLocalization
        case list => Errors.fatal("_ localization methods found for language key _." << (list.length, languageKey))
      }
  }
}

class I18nCatalog(packageSegments: List[String], codeLocalization: I18nCodeLocalization, localizations: List[I18nLocalization]) {

  val codeUsePlural = codeLocalization.usePluralRule
  val codeI18nLanguageKey = codeLocalization.i18nLanguageKey

  // Make sure each localization appears only once.
  I18nUtil.checkUniqueness(codeLocalization, localizations)

  // Try to get any string to force the dynamic class loading.
  localizations.foreach(_.gettext("\uFFFF"))

  // Create a map to find a localization according to its language key represented (as a single string).
  private[core] val map = localizations.map(L => (L.i18nLanguageKey.string, L)).toMap

  // Flag for no localizations
  val noLocalizations = localizations.isEmpty
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

    if (catalog.noLocalizations || (i18nLanguageKey.string eq catalog.codeI18nLanguageKey.string))
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
