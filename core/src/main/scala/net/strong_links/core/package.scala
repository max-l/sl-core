package net.strong_links

import java.util.Locale

package object core 
{

  /**
   * This class is used to pass default implicit parameters to methods, in order to generate distinct internal
   * JVM method names. This is useful when we need to declare two methods with logically the same signature,
   * but with different constraints.
   */
  class DummyParam
  implicit def someDummyParam = new DummyParam

  implicit def stringToPluggableString(s: String) = {
    new PluggableString(s)
  }
  
  implicit def pluggedStringToString(ps: PluggedString) = {
    ps.toString
  }

  implicit def stringToStringErrorParameter(s: String) = {
    new StringErrorParameter(s)
  }

  implicit def pluggedStringToPluggedStringErrorParameter(ps: PluggedString) = {
    new PluggedStringErrorParameter(ps)
  }

  implicit def exceptionToExceptionErrorParameter(e: Exception) = {
    new ExceptionErrorParameter(e)
  }

  implicit def charStreamToSPrintWriter(cs: CharStream) = {
    cs.getPrintWriter
  }
  
  object userLocale extends ThreadLocalStack[Locale]

  implicit def stringToStringGeneralString(s: String): GeneralString = {
    new StringGeneralString(s)
  }

  implicit def i18nToI18nGeneralString(i18n: I18n): GeneralString = {
    new I18nGeneralString(i18n)
  }
}