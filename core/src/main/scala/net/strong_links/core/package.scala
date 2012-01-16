package net.strong_links

import java.util.Locale

package object core {
  /**
   * This class is used to pass default implicit parameters to methods, in order to generate distinct internal
   * JVM method names. This is useful when we need to declare two methods with logically the same signature,
   * but with different constraints.
   */
  class DummyParam0
  implicit def someDummyParam0 = new DummyParam0

  class DummyParam1
  implicit def someDummyParam1 = new DummyParam1

  class DummyParam2
  implicit def someDummyParam2 = new DummyParam2

  implicit def stringToPluggableString(s: String) = {
    new PluggableString(s)
  }

  implicit def pluggedStringToString(ps: PluggedString) = {
    ps.toString
  }

  implicit def stringToStringLoggingParameter(s: String) = {
    new StringLoggingParameter(s)
  }

  implicit def optionStringToStringLoggingParameter(s: Option[String]) = {
    new OptionStringLoggingParameter(s)
  }

  implicit def pluggedStringToPluggedStringLoggingParameter(ps: PluggedString) = {
    new PluggedStringLoggingParameter(ps)
  }

  object userLanguageKey extends ThreadLocalStack[String]

  implicit def stringToStringGeneralString(s: String): GeneralString = {
    new StringGeneralString(s)
  }

  implicit def i18nToI18nGeneralString(i18n: I18n): GeneralString = {
    new I18nGeneralString(i18n)
  }

  def I18n(msgid: String)(implicit catalog: I18nCatalog) =
    new I18n(catalog, None, msgid, None, 0)

  def I18nPlural(msgid: String, msgidPlural: String, n: Int)(implicit catalog: I18nCatalog) =
    new I18n(catalog, None, msgid, Some(msgidPlural), n)

  def I18nCtxt(msgCtxt: String, msgid: String)(implicit catalog: I18nCatalog) =
    new I18n(catalog, Some(msgCtxt), msgid, None, 0)

  def I18nPluralCtxt(msgCtxt: String, msgid: String, msgPlural: String, n: Int)(implicit catalog: I18nCatalog) =
    new I18n(catalog, Some(msgCtxt), msgid, Some(msgPlural), n)
}