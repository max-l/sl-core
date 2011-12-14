package net.strong_links

import java.util.Locale

package object core {
  type Logger = {
    def debug(msg: String): Unit
    def info(msg: String): Unit
    def warning(msg: String): Unit
    def error(msg: String): Unit
  }

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

  implicit def exceptionToExceptionLoggingParameter(e: Exception) = {
    new ExceptionLoggingParameter(e)
  }

  object userLocale extends ThreadLocalStack[Locale]

  implicit def stringToStringGeneralString(s: String): GeneralString = {
    new StringGeneralString(s)
  }

  implicit def i18nToI18nGeneralString(i18n: I18n): GeneralString = {
    new I18nGeneralString(i18n)
  }
}