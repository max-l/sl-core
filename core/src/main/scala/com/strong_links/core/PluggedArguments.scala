package com.strong_links.core

import java.io.File

object PluggedArguments {

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

  private def decorateTraversable(t: Traversable[_], sb: StringBuilder, quoted: Boolean, crashIfException: Boolean) {
    sb.append('[')
    var sep = ""
    for (item <- t) {
      sb.append(sep)
      decorateAny(item, sb, quoted, crashIfException)
      sep = ", "
    }
    sb.append(']')
  }

  private def decorateAny(x: Any, sb: StringBuilder, quoted: Boolean, crashIfException: Boolean) {
    val length = sb.length
    try
      x match {
        case null => sb.append("null")
        case f: File => decorateFile(f, sb, quoted)
        case s: String => decorateString(s, sb, quoted)
        case t: Traversable[_] => decorateTraversable(t, sb, quoted, crashIfException)
        case a: Array[_] =>
          decorateTraversable(a, sb, quoted, crashIfException)
        case option: Option[_] =>
          if (option.isEmpty)
            sb.append("<none>")
          else
            decorateAny(option.get, sb, quoted, crashIfException)
        case _ =>
          decorateString(x.toString, sb, quoted)
      }
    catch {
      case _: Exception if (!crashIfException) => sb.length = length; sb.append("<bad argument>")
      case e: Exception => throw e
    }
  }

  private def safeArg(x: Any): String = {
    val sb = new StringBuilder
    decorateAny(x, sb, true, false)
    sb.toString
  }

  private def format(s: String, args: Array[Any], sb: StringBuilder, quoted: Boolean) {
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
      } else {
        sb.append(current)
      }
      i += 1
    }
    if (argsNotUsedBitmap != 0) {
      // Not all arguments used. Search for first unused one.
      var i = 0
      while (((1 << i) & argsNotUsedBitmap) == 0)
        i += 1
      throw new Exception("Unused argument '___'" << i + 1)
    }
  }

  def format(s: String, args: Option[Seq[Any]], failsafe: Boolean, quoted: Boolean): String = {
    args match {
      case None =>
        s
      case Some(seq) =>
        val sb = new StringBuilder
        try {
          format(s, seq.toArray, sb, quoted)
          sb.toString
        } catch {
          case e: Exception =>
            val msg0 = "Error _ while formating; Format = _; " <<< (e.getMessage, s)
            val msg1 = "arguments = _." << safeArg(seq)
            val msg = msg0 + msg1
            if (failsafe)
              msg
            else
              Errors.fatal(msg)
        }
    }
  }
}