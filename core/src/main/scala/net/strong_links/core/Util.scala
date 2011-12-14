package net.strong_links.core

import java.util.Calendar
import java.text.SimpleDateFormat
import scala.math
import java.security.MessageDigest

object Util {
  private lazy val sdf1 = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
  private lazy val sdf2 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  def nowAsStringRaw: String = {
    sdf1.format(Calendar.getInstance.getTime)
  }

  def nowAsString: String = {
    sdf2.format(Calendar.getInstance.getTime)
  }

  def withStringBuilder(f: StringBuilder => Unit): String = {
    val sb = new StringBuilder
    f(sb)
    sb.toString
  }

  def primesUntil(n: Int, minimum: Int, nbSamples: Int): Array[Int] = {
    val t = Array.fill[Boolean](n)(true)
    t(0) = false; t(1) = false
    for (i <- 2 to math.sqrt(n).toInt; if t(i); j <- 2 to (n / i); val z = i * j; if z < n)
      t(z) = false
    val primes = (0 until t.length).filter(i => t(i) && i >= minimum).toArray
    val sampleDelta = primes.length.toDouble / nbSamples.toDouble
    (for (i <- 0 until nbSamples; val index = (i * sampleDelta).toInt) yield primes(index)).toArray
  }

  def toDoubleList[T](xs: Iterable[T])(implicit numeric: Numeric[T]) = {
    xs.map(numeric.toDouble(_)).toList
  }

  def average[T: Numeric](xs: Iterable[T]): Double = {
    toDoubleList(xs).sum / xs.size
  }

  def averageAndStddev[T: Numeric](xs: Iterable[T]): (Double, Double) = {
    val xl = toDoubleList(xs)
    val n = xl.length
    if (n == 0)
      Errors.fatal("No elements in numeric collection.")
    val avg = xl.sum / n
    val stddev = if (n >= 2) math.sqrt((0.0 /: xl)((a, b) => a + (b - avg) * (b - avg)) / (n - 1)) else 0.0
    (avg, stddev)
  }

  def getLevenshteinDistance(s: String, t: String): Int = {
    import scala.math.min
    if (s == null || t == null)
      Errors.fatal("Strings must not be null")

    val n = s.length
    val m = t.length

    if (n == 0) {
      m
    } else if (m == 0) {
      n
    } else {
      var p = new Array[Int](n + 1)
      var d = new Array[Int](n + 1)

      for (i <- 0 to n)
        p(i) = i

      for (j <- 1 to m) {
        val t_j = t(j - 1)
        d(0) = j
        for (i <- 1 to n)
          d(i) = min(min(d(i - 1) + 1, p(i) + 1), p(i - 1) + (if (s(i - 1) == t_j) 0 else 1))
        val w = p
        p = d
        d = w
      }
      p(n)
    }
  }

  def timerInSeconds(times: Int)(u: => Unit) = {
    val start = System.currentTimeMillis
    (0 until times).foreach(_ => u)
    (System.currentTimeMillis - start) / 1000.0
  }

  def encodeLong(v: Long) = {
    // Every long has 64 bits, and we need 11 segments of 6 bits to represent it.
    // The characters used are letters, digits, underscore and dash.
    val uuidChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"
    (0 until 11).map(i => uuidChars(((v >> (i * 6)) & 63).toInt)).mkString
  }

  // Yes, these masks are required because bytes are signed.
  def toInt(b: Array[Byte], pos: Int): Int = {
    if (pos + 4 > b.length)
      Errors.fatal("Buffer overflow at position _ for length _." << (pos, b.length))
    val w = (b(pos + 0): Int) << 0
    val x = (b(pos + 1): Int) << 8
    val y = (b(pos + 2): Int) << 16
    val z = (b(pos + 3): Int) << 24
    (z & 0xFF000000) | (y & 0x00FF0000) | (x & 0x0000FF00) | (w & 0x000000FF)
  }

  // Yes, these masks are required because integers are signed.
  def toLong(b: Array[Byte], pos: Int): Long = {
    val y = (toInt(b, pos): Long) << 0
    val z = (toInt(b, pos + 4): Long) << 32
    (z & 0xFFFFFFFF00000000L) | (y & 0x00000000FFFFFFFFL)
  }

  def encodeLongFromBytes(b: Array[Byte], pos: Int): String = {
    encodeLong(toLong(b, pos))
  }

  def encodeLongFromBytes(b: Array[Byte]): String = {
    if (b.length != 8)
      Errors.fatal("Buffer has a length _ while 8 was expected." << b.length)
    encodeLongFromBytes(b, 0)
  }

  def newGuid = {
    import java.util.UUID
    val uuid = UUID.randomUUID
    encodeLong(uuid.getMostSignificantBits) + encodeLong(uuid.getLeastSignificantBits)
  }

  trait UUIDIdentified {
    val uuid: String = newGuid
  }

  def filterOn[T](list: List[_])(implicit m: Manifest[T]) =
    list.filter(m.erasure.isInstance(_)).map(_.asInstanceOf[T])

  def newMd5Function = new {
    val md5 = MessageDigest.getInstance("MD5")

    def apply(b: Array[Byte]*) = {
      b.foreach(md5.update(_))
      val result = md5.digest
      md5.reset
      result
    }
  }

  def checkDuplicates[T](I: Iterable[T])(errorCode: T => Unit) {
    val set = scala.collection.mutable.Set[T]()
    for (e <- I) {
      if (set.contains(e))
        errorCode(e)
      set += e
    }
  }

  private def genericSplitTwo(s: String, delimiter: Char, originalDelimiter: => String, ctx: Option[() => LoggingParameter]) = {
    def error(params: LoggingParameter*) = {
      val errorArgs = (("Split error on input string _" <<< s): LoggingParameter) +: ctx.toList.map(_()) ::: params.toList
      Errors.fatal(errorArgs: _*)
    }
    if (!s.contains(delimiter))
      error("Delimiter _ not found." << originalDelimiter)
    val x = s.split(delimiter)
    if (x.length != 2)
      error("Wrong number of segments", "Found _, expected 2" << x.length)
    (x(0), x(1))
  }

  // Warning: the Java split function using a string delimiter uses a regex delimiter, so bypass this weird behavior.
  def splitTwoStr(s: String, delimiter: String, ctx: Option[() => LoggingParameter] = None) =
    genericSplitTwo(s.replace(delimiter, "\uFFFF"), '\uFFFF', delimiter, ctx)

  def splitTwoCh(s: String, delimiter: Char, ctx: Option[() => LoggingParameter] = None) = {
    genericSplitTwo(s, delimiter, delimiter.toString, ctx)
  }
}