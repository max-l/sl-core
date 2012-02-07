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

  private def splitAndclean(specifications: String) = Util.split(specifications, ",").map(_.trim).filter(!_.isEmpty)

  private def toLocale(key: String) = I18nLocale.from(key).locale

  def toLocaleSeq(specifications: String) = splitAndclean(specifications).map(toLocale).toSeq

  def toMappings(specifications: String) = (for (
    p <- splitAndclean(specifications);
    (from, to) = Util.splitTwoTrimmed(p, "->")
  ) yield toLocale(from) -> toLocale(to)).toMap
}

class I18nConfig(val packageName: String, val i18nCodeLocalization: I18nStock,
  fullLocales: Seq[Locale], deltaLocales: Seq[Locale], _mappings: Map[Locale, Locale]) {

  def this(packageName: String, codeKey: String, masterKeys: String = "", subKeys: String = "", mappings: String = "") =
    this(packageName, I18nStock.get(codeKey),
      I18nConfig.toLocaleSeq(masterKeys), I18nConfig.toLocaleSeq(subKeys),
      I18nConfig.toMappings(mappings))

  def this(packageName: String, i18nCodeLocalization: I18nStock,
    fullLocales: Seq[Locale], deltaLocales: Seq[Locale]) =
    this(packageName, i18nCodeLocalization, fullLocales, deltaLocales, Map[Locale, Locale]())

  def this(packageName: String, i18nCodeLocalization: I18nStock,
    fullLocales: Seq[Locale]) =
    this(packageName, i18nCodeLocalization, fullLocales, Seq[Locale](), Map[Locale, Locale]())

  def this(packageName: String, i18nCodeLocalization: I18nStock) =
    this(packageName, i18nCodeLocalization, Seq[Locale](), Seq[Locale](), Map[Locale, Locale]())

  val packageNameSegments = I18nConfig.toPackageSegments(packageName)

  val fullI18nLocales = fullLocales.toList.map(I18nLocale.apply).sorted

  val deltaI18nLocales = deltaLocales.toList.map(I18nLocale.apply).sorted

  def externalI18nLocales = fullI18nLocales ::: deltaI18nLocales

  private val allI18nLocales = i18nCodeLocalization.i18nLocale :: externalI18nLocales

  val mappingsList = _mappings.toList.map(e => (I18nLocale(e._1), I18nLocale(e._2))).sorted
  val mappings = mappingsList.toMap

  // Ensure that no mapping is duplicated.
  mappingsList.groupBy(_._1).filter(_._2.length > 1).map(_._1) match {
    case Nil =>
    case dups => Errors.fatal("Duplicate mappings _." << dups)
  }

  // Ensure that no mapping source is already known.
  mappingsList.map(_._1).filter(e => allI18nLocales.exists(_ == e)) match {
    case Nil =>
    case list => Errors.fatal("Invalid mapping sources _." << list)
  }

  // Ensure that all mapping targets are known.
  mappingsList.map(_._2).filterNot(e => allI18nLocales.exists(_ == e)) match {
    case Nil =>
    case list => Errors.fatal("Unknown mapping targets _." << list)
  }

  def serialize = {
    def q(s: String) = "\"" + s.replace("\"", "\\\"") + "\""
    def x(l: Seq[I18nLocale]) = q(l.map(_.key).mkString(","))
    def z(l: List[(I18nLocale, I18nLocale)]) = q(l.map(e => e._1.key + "->" + e._2.key).mkString(","))
    List(q(packageName), q(i18nCodeLocalization.i18nLocale.key),
      x(fullI18nLocales), x(deltaI18nLocales), z(mappingsList)).mkString((", "))
  }

  def resolve(optionI18nLocale: Option[I18nLocale]): Option[I18nLocale] =
    optionI18nLocale match {
      case None => None
      case Some(i18nLocale) =>
        if (i18nLocale == i18nCodeLocalization.i18nLocale)
          None
        else if (externalI18nLocales.contains(i18nLocale))
          Some(i18nLocale)
        else
          mappings.get(i18nLocale) match {
            case None => resolve(i18nLocale.down)
            case target => resolve(target)
          }
    }

  def getLocalizations(i18nLocale: I18nLocale) = {
    var previousSet = false
    var previous: Option[I18nLocale] = None
    def makeChain(optionI18nLocale: Option[I18nLocale]): List[Option[I18nLocale]] =
      resolve(optionI18nLocale) match {
        case None => Nil
        case Some(x) => Some(x) :: makeChain(x.down)
      }
    val r = makeChain(Some(i18nLocale)) :+ None // None means "code" here.
    val list = for (
      e <- r if !previousSet || e != previous
    ) yield {
      previousSet = true
      previous = e
      e
    }
    if (list != list.distinct)
      Errors.fatal("Bad logic, list = _, list.distinct = _" << (list, list.distinct))
    if (list.last != None)
      Errors.fatal("Bad logic, list _ does not end with None (meaning code localization)." << list)
    list
  }

  private def fmtTarget(targetI18nLocale: Option[I18nLocale]) = targetI18nLocale match {
    case None => i18nCodeLocalization.i18nLocale.key + " (code)"
    case Some(x) => x.key
  }

  def getLocalizationsFmt(i18nLocale: I18nLocale) = {
    getLocalizations(i18nLocale).map(fmtTarget)
  }

  def showConfig {

    def fmtList(list: List[I18nLocale]): String = if (list == Nil) "None" else "_" <<< list
    def fmtMap(list: List[String]): String = if (list == Nil) "None" else "_" << list

    println
    println("Configuration for package _" <<< packageName)
    println("  Code locale: _" <<< i18nCodeLocalization.i18nLocale.key)
    println("  Full locales: _" << fmtList(fullI18nLocales))
    println("  Delta locales: _" << fmtList(deltaI18nLocales))
    def fmtTuple(t: (I18nLocale, I18nLocale)): String = "_ -> _" <<< (t._1, t._2)
    println("  Mappings: _" << fmtMap(mappingsList.map(fmtTuple)))
    println("  Catalog resolutions")
    val localesToResolve = (allI18nLocales ::: mappingsList.map(_._1)).sorted
    localesToResolve.foreach(r => println("    - Locale _ would use the chained localizations _" <<< (r, getLocalizationsFmt(r))))
  }
}

