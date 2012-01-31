package com.strong_links.core

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

private[core] class I18nConfigLocalization(val i18nlocale: I18nLocale, val parentLocale: Option[I18nConfigLocalization]) {
  override def toString = parentLocale match {
    case None => i18nlocale.toString
    case Some(pl) => i18nlocale + ":" + pl
  }
}

class I18nConfig2(packageName: String, i18nKnownLocalization: I18nKnownLocalization,
  masterLocales: Seq[Locale] = Nil, subLocales: Seq[Locale] = Nil) {

  val packageNameSegments = I18nConfig.toPackageSegments(packageName)

  val (masterConfigLocalizations, subConfigLocalizations) = {

    implicit val i18nConfigLocalizationComparer = new Ordering[I18nConfigLocalization] {
      def compare(a: I18nConfigLocalization, b: I18nConfigLocalization): Int = a.i18nlocale compare b.i18nlocale
    }

    val masterI18nLocales = masterLocales.map(I18nLocale.apply)
    val subI18nLocales = subLocales.map(I18nLocale.apply)

    val codeI18nLocale = I18nLocale(i18nKnownLocalization.locale)
    val codeI18nConfigLocalization = new I18nConfigLocalization(codeI18nLocale, None)

    val groups = (codeI18nLocale +: (masterI18nLocales union subI18nLocales)).groupBy(_.key).toList
    groups.filter(_._2.length > 1).map(_._1) match {
      case Nil =>
      case duplicates => Errors.fatal("The duplicate locales have been provided: _" << duplicates)
    }

    val masters = masterI18nLocales.map(loc => new I18nConfigLocalization(loc, None))

    val possibleParents = scala.collection.mutable.Map[I18nLocale, I18nConfigLocalization]()

    def add(i18nLocale: Option[I18nLocale]): Unit = i18nLocale match {
      case None =>
      case Some(loc) =>
        possibleParents += (loc -> codeI18nConfigLocalization)
        add(loc.down)
    }

    def searchParent(i18nLocale: Option[I18nLocale]): Option[I18nConfigLocalization] = i18nLocale match {
      case None => None
      case Some(loc) => possibleParents.get(loc) match {
        case Some(m) => Some(m)
        case None => searchParent(loc.down)
      }
    }

    // At this point, possible parents are the master localizations.
    (possibleParents /: masterI18nLocales)((s, ml) => s += (ml -> new I18nConfigLocalization(ml, None)))

    // We also add all the possible flavors of the code localization.
    add(Some(codeI18nLocale))

    val subs = subI18nLocales.sortWith(I18nLocale.sort).map(loc => {
      val x = searchParent(loc.down) match {
        case None => Errors.fatal("No parent localization found for _." << loc)
        case Some(parent) if parent.i18nlocale == codeI18nLocale =>
          new I18nConfigLocalization(loc, None)
        case Some(parent) => {
          new I18nConfigLocalization(loc, Some(parent))
        }
      }
      // Once a sub has been resolved, it can also be the parent of another sub.
      possibleParents += (loc -> x)
      x
    })

    println("Code: _" << codeI18nConfigLocalization)

    (masters.sorted, subs.sorted)
  }

  println("Masters: _" << masterConfigLocalizations)
  println("Subs: _" << subConfigLocalizations)
}

