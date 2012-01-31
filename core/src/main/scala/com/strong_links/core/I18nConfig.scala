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

  def toLocaleSeq(specifications: String) =
    Util.split(specifications, ",").map(_.trim).filter(!_.isEmpty).map(L => I18nLocale.from(L).locale).toSeq
}

class I18nConfigLocalization(val i18nLocale: I18nLocale, val parentLocale: Option[I18nConfigLocalization]) {

  override def toString = parentLocale match {
    case None => i18nLocale.toString
    case Some(pl) => i18nLocale + ":" + pl
  }

  def classNameFor(packageNameSegments: List[String]) = (i18nLocale.key :: packageNameSegments).mkString("_")
}

class I18nConfig(val packageName: String, val i18nKnownLocalization: I18nKnownLocalization,
  val masterLocales: Seq[Locale] = Nil, val subLocales: Seq[Locale] = Nil) {

  def this(packageName: String, codeKey: String, masterKeys: String = "", subKeys: String = "") =
    this(packageName, I18nKnownLocalization.get(codeKey), I18nConfig.toLocaleSeq(masterKeys), I18nConfig.toLocaleSeq(subKeys))

  def serialize = {
    def q(s: String) = "\"" + s.replace("\"", "\\\"") + "\""
    def x(l: Seq[Locale]) = q(l.map(_.toString).mkString(","))
    List(q(packageName), q(i18nKnownLocalization.i18nLocale.key), x(masterLocales), x(subLocales)).mkString((", "))
  }

  val packageNameSegments = I18nConfig.toPackageSegments(packageName)

  val (masterConfigLocalizations, subConfigLocalizations) = {

    val i18nLocaleComparer = new Ordering[I18nLocale] {
      def compare(a: I18nLocale, b: I18nLocale): Int = a compare b
    }

    val masterI18nLocales = masterLocales.map(I18nLocale.apply).toList.sorted(i18nLocaleComparer)
    val subI18nLocales = subLocales.map(I18nLocale.apply).toList.sorted(i18nLocaleComparer)

    val codeI18nLocale = I18nLocale(i18nKnownLocalization.locale)
    val codeI18nConfigLocalization = new I18nConfigLocalization(codeI18nLocale, None)

    val groups = (codeI18nLocale +: (masterI18nLocales union subI18nLocales)).groupBy(_.key).toList
    groups.filter(_._2.length > 1).map(_._1) match {
      case Nil =>
      case duplicates => Errors.fatal("These duplicate locales have been provided: _" << duplicates)
    }

    val masters = masterI18nLocales.map(loc => new I18nConfigLocalization(loc, None))

    type Parent = (I18nLocale, Int, I18nConfigLocalization)
    val SubWeight = 0
    val CodeWeight = 1
    val MasterWeight = 2

    def makePossibleParents(i18nLocale: Option[I18nLocale], weight: Int, i18nConfigLocalization: I18nConfigLocalization): List[Parent] = i18nLocale match {
      case None => Nil
      case Some(loc) =>
        (loc, weight, i18nConfigLocalization) +: makePossibleParents(loc.down, weight, i18nConfigLocalization)
    }

    def cmp(x: Parent, y: Parent) = {
      val (loc1, w1, c1) = x
      val (loc2, w2, c2) = y
      ((loc1 compare loc2) match {
        case 0 => w1 compare w2
        case x => -x
      }) < 0
    }

    var possibleParents = (makePossibleParents(Some(codeI18nLocale), CodeWeight, codeI18nConfigLocalization) :::
      masters.flatMap(m => makePossibleParents(Some(m.i18nLocale), MasterWeight, m))).sortWith(cmp)

    def searchParent(i18nLocale: Option[I18nLocale]): Option[I18nConfigLocalization] = i18nLocale match {
      case None => None
      case Some(loc) => possibleParents.find(loc == _._1) match {
        case None => searchParent(loc.down)
        case Some(x) => Some(x._3)
      }
    }

    val subs = subI18nLocales.sortWith(I18nLocale.sort).map(loc => {
      val x = searchParent(loc.down) match {
        case None => Errors.fatal("No parent localization found for _." << loc)
        case Some(parent) if parent == codeI18nConfigLocalization =>
          new I18nConfigLocalization(loc, None)
        case Some(parent) => {
          new I18nConfigLocalization(loc, Some(parent))
        }
      }
      // Once a sub has been resolved, it can also be the parent of another sub.
      possibleParents +:= (loc, SubWeight, x)
      x
    })

    (masters, subs)
  }

  def toCatalog = new I18nCatalog(packageName, i18nKnownLocalization, masterConfigLocalizations, subConfigLocalizations)

  def allLocalizations = masterConfigLocalizations ::: subConfigLocalizations
}

