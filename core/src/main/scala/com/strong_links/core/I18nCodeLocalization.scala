package com.strong_links.core

import java.util.Locale

class I18nCodeLocalization(packageSegments: List[String], i18nLanguageKey: I18nLanguageKey, val usePluralRule: (Int) => Boolean, val usePluralRulePoString: String)
  extends I18nLocalization(packageSegments, i18nLanguageKey, None) {
  val locale = new Locale(i18nLanguageKey.language, i18nLanguageKey.country.getOrElse(""))
}

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

