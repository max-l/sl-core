package com.strong_links.core

import java.util.Locale

class I18nKnownLocalization(val locale: Locale, val rule: (Int) => Boolean, val poRule: String) {

  val i18nLocale = I18nLocale(locale)

  def cloneFor(locale: Locale): I18nKnownLocalization = {
    if (this.locale.getLanguage != locale.getLanguage)
      Errors.fatal("Known localization _ and provided localization _ have different languages _ and _." <<
        (this.locale, locale, this.locale.getLanguage, locale.getLanguage))
    new I18nKnownLocalization(locale, rule, poRule)
  }

  def cloneFor(languageKey: String): I18nKnownLocalization = cloneFor(I18nLocale.from(languageKey).locale)
}

// Thanks to people behind "http://translate.sourceforge.net/wiki/l10n/pluralforms"
object I18nKnownLocalization {

  val rule00 = ((n: Int) => false, "nplurals=1; plural=0")
  val rule01 = ((n: Int) => n > 1, "nplurals=2; plural=n > 1")
  val rule02 = ((n: Int) => n != 1, "nplurals=2; plural=n != 1")
  val rule03 = ((n: Int) => n != 0, "nplurals=2; plural=n != 0")
  val rule04 = ((n: Int) => (n % 10 != 1) || (n % 100 == 11), "nplurals=2; plural=(n % 10 != 1) || (n % 100 == 11)")

  val map = Map(
    "ach" -> rule01,
    "af" -> rule02,
    "ak" -> rule01,
    "am" -> rule01,
    "an" -> rule02,
    "arn" -> rule01,
    "ast" -> rule02,
    "ay" -> rule00,
    "az" -> rule02,
    "bg" -> rule02,
    "bn" -> rule02,
    "bo" -> rule00,
    "br" -> rule01,
    "ca" -> rule02,
    "cgg" -> rule00,
    "da" -> rule02,
    "de" -> rule02,
    "de_AT" -> rule02,
    "de_CH" -> rule02,
    "de_DE" -> rule02,
    "dz" -> rule00,
    "el" -> rule02,
    "en" -> rule02,
    "en_UK" -> rule02,
    "en_US" -> rule02,
    "eo" -> rule02,
    "es" -> rule02,
    "et" -> rule02,
    "eu" -> rule02,
    "fa" -> rule00,
    "fi" -> rule02,
    "fil" -> rule01,
    "fo" -> rule02,
    "fr" -> rule01,
    "fr_BE" -> rule01,
    "fr_CA" -> rule01,
    "fr_FR" -> rule01,
    "fur" -> rule02,
    "fy" -> rule02,
    "gl" -> rule02,
    "gu" -> rule02,
    "gun" -> rule01,
    "ha" -> rule02,
    "he" -> rule02,
    "hi" -> rule02,
    "hy" -> rule02,
    "hu" -> rule02,
    "ia" -> rule02,
    "id" -> rule00,
    "is" -> rule04,
    "it" -> rule02,
    "it_CH" -> rule02,
    "it_IT" -> rule02,
    "ja" -> rule00,
    "jbo" -> rule00,
    "jv" -> rule03,
    "ka" -> rule00,
    "kk" -> rule00,
    "km" -> rule00,
    "kn" -> rule02,
    "ko" -> rule00,
    "ku" -> rule02,
    "ky" -> rule00,
    "lb" -> rule02,
    "ln" -> rule01,
    "lo" -> rule00,
    "mai" -> rule02,
    "mfe" -> rule01,
    "mg" -> rule01,
    "mi" -> rule01,
    "ml" -> rule02,
    "mn" -> rule02,
    "mr" -> rule02,
    "ms" -> rule00,
    "nah" -> rule02,
    "nap" -> rule02,
    "nb" -> rule02,
    "ne" -> rule02,
    "nl" -> rule02,
    "se" -> rule02,
    "nn" -> rule02,
    "no" -> rule02,
    "nso" -> rule02,
    "oc" -> rule01,
    "or" -> rule02,
    "ps" -> rule02,
    "pa" -> rule02,
    "pap" -> rule02,
    "pms" -> rule02,
    "pt" -> rule02,
    "pt_BR" -> rule01,
    "rm" -> rule02,
    "sah" -> rule00,
    "sco" -> rule02,
    "si" -> rule02,
    "so" -> rule02,
    "son" -> rule02,
    "sq" -> rule02,
    "su" -> rule00,
    "sw" -> rule02,
    "sv" -> rule02,
    "ta" -> rule02,
    "te" -> rule02,
    "tg" -> rule00,
    "ti" -> rule01,
    "th" -> rule00,
    "tk" -> rule02,
    "tr" -> rule01,
    "tt" -> rule00,
    "ug" -> rule00,
    "ur" -> rule02,
    "uz" -> rule00,
    "vi" -> rule00,
    "wa" -> rule01,
    "wo" -> rule00,
    "yo" -> rule02,
    "zh" -> rule00,
    "zh2" -> rule01)

  def get(key: String) = map.get(key) match {
    case None => Errors.fatal("Localization _ is unknown." << key)
    case Some(x) => new I18nKnownLocalization(I18nLocale.from(key).locale, x._1, x._2)
  }

  def getBest(key: String): Option[I18nKnownLocalization] = {
    var loc: Option[I18nLocale] = Some(I18nLocale.from(key))
    var results: Option[I18nKnownLocalization] = None
    while (loc != None && results == None)
      if (map.contains(loc.get.key))
        results = Some(get(loc.get.key))
      else
        loc = loc.get.down
    return results
  }

  def ach = get("ach")
  def af = get("af")
  def ak = get("ak")
  def am = get("am")
  def an = get("an")
  def arn = get("arn")
  def ast = get("ast")
  def ay = get("ay")
  def az = get("az")
  def bg = get("bg")
  def bn = get("bn")
  def bo = get("bo")
  def br = get("br")
  def ca = get("ca")
  def cgg = get("cgg")
  def da = get("da")
  def de = get("de")
  def de_AT = get("de_AT")
  def de_CH = get("de_CH")
  def de_DE = get("de_DE")
  def dz = get("dz")
  def el = get("el")
  def en = get("en")
  def en_UK = get("en_UK")
  def en_US = get("en_US")
  def eo = get("eo")
  def es = get("es")
  def et = get("et")
  def eu = get("eu")
  def fa = get("fa")
  def fi = get("fi")
  def fil = get("fil")
  def fo = get("fo")
  def fr = get("fr")
  def fr_BE = get("fr_BE")
  def fr_CA = get("fr_CA")
  def fr_FR = get("fr_FR")
  def fur = get("fur")
  def fy = get("fy")
  def gl = get("gl")
  def gu = get("gu")
  def gun = get("gun")
  def ha = get("ha")
  def he = get("he")
  def hi = get("hi")
  def hy = get("hy")
  def hu = get("hu")
  def ia = get("ia")
  def id = get("id")
  def is = get("is")
  def it = get("it")
  def it_CH = get("it_CH")
  def it_IT = get("it_IT")
  def ja = get("ja")
  def jbo = get("jbo")
  def jv = get("jv")
  def ka = get("ka")
  def kk = get("kk")
  def km = get("km")
  def kn = get("kn")
  def ko = get("ko")
  def ku = get("ku")
  def ky = get("ky")
  def lb = get("lb")
  def ln = get("ln")
  def lo = get("lo")
  def mai = get("mai")
  def mfe = get("mfe")
  def mg = get("mg")
  def mi = get("mi")
  def mk = get("mk")
  def ml = get("ml")
  def mn = get("mn")
  def mr = get("mr")
  def ms = get("ms")
  def nah = get("nah")
  def nap = get("nap")
  def nb = get("nb")
  def ne = get("ne")
  def nl = get("nl")
  def se = get("se")
  def nn = get("nn")
  def no = get("no")
  def nso = get("nso")
  def oc = get("oc")
  def or = get("or")
  def ps = get("ps")
  def pa = get("pa")
  def pap = get("pap")
  def pms = get("pms")
  def pt = get("pt")
  def pt_BR = get("pt_BR")
  def rm = get("rm")
  def sah = get("sah")
  def sco = get("sco")
  def si = get("si")
  def so = get("so")
  def son = get("son")
  def sq = get("sq")
  def su = get("su")
  def sw = get("sw")
  def sv = get("sv")
  def ta = get("ta")
  def te = get("te")
  def tg = get("tg")
  def ti = get("ti")
  def th = get("th")
  def tk = get("tk")
  def tr = get("tr")
  def tt = get("tt")
  def ug = get("ug")
  def ur = get("ur")
  def uz = get("uz")
  def vi = get("vi")
  def wa = get("wa")
  def wo = get("wo")
  def yo = get("yo")
  def zh = get("zh")
  def zh2 = get("zh2")
}

