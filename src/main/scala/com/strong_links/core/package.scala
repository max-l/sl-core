package com.strong_links

import java.util.Locale
import java.io.File

package object core extends I18nImplicits {

  implicit def stringToPluggableString(s: String) = new PluggableString(s)

  implicit def pluggedStringToString(pluggedString: PluggedString) = pluggedString.f

  implicit val i18nLocaleComparer = new Ordering[I18nLocale] {
    def compare(a: I18nLocale, b: I18nLocale): Int = a compare b
  }

  implicit def stringToStringLoggingParameter(s: String) = new StringLoggingParameter(s)

  implicit def optionStringToStringLoggingParameter(s: Option[String]) = new OptionStringLoggingParameter(s)

  implicit def pluggedStringToPluggedLoggingParameter(pluggedString: PluggedString) =
    new PluggedStringLoggingParameter(pluggedString)

  implicit def fileToFileLoggingParameter(f: File) = new FileLoggingParameter(f)

  implicit def FileToBetterFile(file: File) = new BetterFile(file)

  class DoubleFormatter(d: Double) {
    def f2 = d.formatted("%.2f")
  }

  implicit def doubleToDoubleFormatter(d: Double) = new DoubleFormatter(d)

  class Wrapper[R](r: R) {

    def spy = {
      Console.err.println("Spy --\n/_/" << r)
      r
    }

    def spy(what: LoggingParameter*) = {
      Console.err.println("Spy _ -- \n/_/" << (LoggingParameter.format(what), r))
      r
    }
  }

  implicit def anyToWrapper[R](r: R) = new Wrapper[R](r)
}