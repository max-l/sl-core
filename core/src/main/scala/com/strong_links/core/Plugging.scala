package com.strong_links.core

class PluggableString private[core] (s: String) {

  def <<(args: Any*) = new PluggedString(s, args, false)

  def <<<(args: Any*) = new PluggedString(s, args, true)
}

class PluggedString private[core] (s: String, args: Seq[_], quotedDefault: Boolean) {

  def fmt(safe: Boolean, quoted: Boolean) = Plugging.format(s, args, safe, quoted)

  def f = fmt(false, quotedDefault)

  override def toString = f
}

class PluggableI18n private[core] (catalog: I18nCatalog, val msgCtxt: String, val msgid: String, val msgidPlural: String, val n: Int)
  extends I18n {

  // This is the key used for searches in the resource files. Strings must be "internized", as this will allow fast
  // identity lookups.
  lazy val key = I18nUtil.compute(msgCtxt, msgid).intern

  def f(implicit i18nLocale: I18nLocale) = catalog.translate(this, i18nLocale)

  override def toString = f(I18nLocale.systemCurrent)

  def <<(args: Any*) = new PluggedI18n(this, args, false)

  def <<<(args: Any*) = new PluggedI18n(this, args, true)
}

class PluggedI18n private[core] (pluggableI18n: PluggableI18n, args: Seq[_], quotedDefault: Boolean) extends I18n {

  def key = pluggableI18n.key

  def fmt(safe: Boolean, quoted: Boolean)(implicit i18nLocale: I18nLocale) =
    Plugging.format(pluggableI18n.f(i18nLocale), args, safe, quoted)

  def f(implicit i18nLocale: I18nLocale) = fmt(false, quotedDefault)(i18nLocale)

  override def toString = f(I18nLocale.systemCurrent)
}

private[core] object Plugging {

  import java.io.File

  private val quote = '\''

  private def decorateFile(f: File, sb: StringBuilder, quoted: Boolean) {
    if (f.isDirectory)
      sb.append("Directory ")
    else
      sb.append("File ")
    if (quoted)
      sb.append(quote)
    sb.append(f.path)
    if (quoted)
      sb.append(quote)
  }

  private def decorateString(s: String, sb: StringBuilder, quoted: Boolean) {
    if (quoted)
      sb.append(quote)
    sb.append(s)
    if (quoted)
      sb.append(quote)
  }

  private def decorateTraversable(t: Traversable[_], sb: StringBuilder, quoted: Boolean, crash: Boolean) {
    sb.append('[')
    var sep = ""
    for (item <- t) {
      sb.append(sep)
      decorateAny(item, sb, quoted, crash)
      sep = ", "
    }
    sb.append(']')
  }

  private def decorateOption(option: Option[_], sb: StringBuilder, quoted: Boolean, crash: Boolean) {
    if (option.isEmpty)
      sb.append("<none>")
    else
      decorateAny(option.get, sb, quoted, crash)
  }

  private def decorateAny(x: Any, sb: StringBuilder, quoted: Boolean, crash: Boolean) {
    val length = sb.length
    try
      x match {
        case null => sb.append("null")
        case f: File => decorateFile(f, sb, quoted)
        case s: String => decorateString(s, sb, quoted)
        case t: Traversable[_] => decorateTraversable(t, sb, quoted, crash)
        case a: Array[_] => decorateTraversable(a, sb, quoted, crash)
        case option: Option[_] => decorateOption(option, sb, quoted, crash)
        case _ => decorateString(x.toString, sb, quoted)
      }
    catch {
      case _ if (!crash) => sb.length = length; sb.append("<bad argument>")
      case t => throw t
    }
  }

  private def safeArg(x: Any): String = {
    val sb = new StringBuilder
    decorateAny(x, sb, true, false)
    sb.toString
  }

  private def format(s: String, args: Seq[_], sb: StringBuilder, quoted: Boolean) {
    var i = 0
    var n = args.length
    if (n > 32)
      throw new Exception("Too many arguments;_ were provided while the maximum is 32" << n)
    var argsNotUsedBitmap = (1 << n) - 1
    var previousArgNo = -1
    val lastIndex = s.length - 1
    while (i <= lastIndex) {
      val current = s(i)
      if (current == '_') {
        if ((i != lastIndex) && (s(i + 1) == '_')) {
          sb.append('_')
          i += 1
        } else {
          // If there is a '!' before a '_', then we force quoting to false and we remove the '!' from the emitted buffer.
          val actualQuoted = if (i > 0 && s(i - 1) == '!') {
            sb.length -= 1
            false
          } else
            quoted
          val argNo =
            if ((i != lastIndex) && s(i + 1).isDigit) {
              var k = 0
              // Argument number specified in the string, use it.
              do {
                i += 1
                k = (k * 10) + (s(i) - '0')
              } while ((i != lastIndex) && s(i + 1).isDigit)
              // Argument 0 is invalid since we are 1-based externally.
              if (k == 0)
                throw new Exception("Invalid argument '_0' specified (arguments are one-based, not zero-based)")
              // Internally, we are 0-based.
              k - 1
            } else {
              previousArgNo + 1
            }
          // Mark this argument as used by clearing its bit in the bitmap.
          argsNotUsedBitmap &= ~(1 << argNo)

          // Make sure we do not overflow.
          if (argNo >= n)
            throw new Exception("Missing argument '___'" << argNo + 1)

          // Got it!
          decorateAny(args(argNo), sb, actualQuoted, true)

          // Get ready for next iteration.
          previousArgNo = argNo
        }
      } else
        sb.append(current)
      i += 1
    }
    // If not all arguments used, show the first unused one.
    if (argsNotUsedBitmap != 0) {
      for (i <- Stream.from(0); if ((1 << i) & argsNotUsedBitmap) != 0)
        throw new Exception("Unused argument '___'" << i + 1)
    }
  }

  def format(s: String, args: Seq[_], safe: Boolean, quoted: Boolean): String =
    if (args.size == 0)
      s
    else {
      val sb = new StringBuilder
      try {
        format(s, args, sb, quoted)
        sb.toString
      } catch {
        case e: Exception =>
          val msg0 = "Error _ while formating; Format = _; " <<< (e.getMessage, s)
          val msg1 = "arguments = _." << safeArg(args)
          val msg = msg0 + msg1
          if (safe)
            msg
          else
            Errors.fatal(msg)
      }
    }
}
