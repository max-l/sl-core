package com.strong_links.core

import scala.collection._
import java.util.Calendar
import java.text.SimpleDateFormat
import java.util.regex.Pattern
import scala.math
import java.security.MessageDigest

object Util {
  lazy val calendar = Calendar.getInstance

  def getTime = calendar.getTime

  private lazy val sdf1 = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
  private lazy val sdf2 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  private lazy val sdf3 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ssZ")
  private lazy val sdf4 = new SimpleDateFormat("HH:mm:ss:SSS")

  def now = System.currentTimeMillis

  def nowAsStringRaw: String = {
    sdf1.format(getTime)
  }

  def nowAsString: String = {
    sdf2.format(getTime)
  }

  def nowAsStringWithTimeDelta: String = {
    sdf3.format(getTime)
  }

  def nowForLogging: String = {
    sdf4.format(getTime)
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

  def getWeightedLevenshteinDistance(from: String, to: String): Double = {
    val cost: Double = Util.getLevenshteinDistance(from, to)
    val L = to.length
    if (L == 0) Double.PositiveInfinity else cost / L
  }

  def timerInSeconds(times: Long)(u: => Any): Double = {
    val start = System.nanoTime
    var i: Long = 0
    while (i < times) {
      u
      i += 1
    }
    val stop = System.nanoTime
    val delta = stop - start
    (delta).toDouble / 1000000000.0
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
    list.filter(m.erasure.isInstance).map(_.asInstanceOf[T])

  def md5 = new {
    private val f = MessageDigest.getInstance("MD5")

    def apply(b: Array[Byte]*) = {
      b.foreach(f.update)
      val result = f.digest
      f.reset
      result
    }
  }

  def strongHash(s: String) = {
    val bytes = md5(s.getBytes)
    toLong(bytes, 0)
  }

  def findDuplicates[A, C](coll: C)(implicit c2i: C => Iterable[A], cbf: generic.CanBuildFrom[C, A, C]): C =
    if (coll.hasDefiniteSize) {
      val builder = cbf()
      val seen = mutable.Set[A]()
      for (item <- coll) {
        if (seen(item))
          builder += item
        seen += item
      }
      builder.result
    } else
      Errors.fatal("Collection has an infinite size.")

  def findDuplicatesOption[A, C](coll: C)(implicit c2i: C => Iterable[A], cbf: generic.CanBuildFrom[C, A, C]): Option[C] =
    findDuplicates(coll)(c2i, cbf) match {
      case Nil => None
      case list => Some(list)
    }

  def split(s: String, del: String): List[String] = s.split(Pattern.quote(del)).toList

  // Trim right a single line.
  def trimRightLine(s: String) = {
    var L = s.length
    while (L > 0 && s(L - 1).isWhitespace)
      L -= 1
    if (L == s.length)
      s
    else
      s.substring(0, L)
  }

  // Trim right a number of input lines.
  def trimRight(s: String) = if (s.contains('\n'))
    split(s, '\n').map(trimRightLine).mkString("\n")
  else
    trimRightLine(s)

  def split(s: String, del: Char): List[String] = split(s, del.toString)

  def nsplit(s: String, nbExpected: Int, del: String): List[String] = {
    val segments = split(s, del)
    if (segments.length != nbExpected)
      Errors.fatal("_ segments found, _ expected" << (segments.length, nbExpected),
        "String _, delimiter _" << (s, del))
    segments
  }

  def nsplit(s: String, nbExpected: Int, del: Char): List[String] =
    nsplit(s, nbExpected, del.toString)

  def splitTwo(s: String, del: String): (String, String) = {
    val list = nsplit(s, 2, del)
    (list(0), list(1))
  }

  def splitTwoTrimmed(s: String, del: String): (String, String) = {
    val list = nsplit(s, 2, del)
    (list(0).trim, list(1).trim)
  }

  def splitTwo(s: String, del: Char): (String, String) =
    splitTwo(s, del.toString)

  def splitTwoTrimmed(s: String, del: Char): (String, String) =
    splitTwoTrimmed(s, del.toString)

  def sp(singular: String, plural: String, n: Int) = if (n == 1) singular else plural

  trait FullyQualifiedName {
    val fqn = {
      def keep(s: String) = !s.isEmpty && (try { s.toInt; false } catch { case _ => true })
      Util.split(getClass.getName.replace('$', '.'), '.').filter(keep).mkString(".")
    }
    lazy val fqnStrongHash = Util.strongHash(fqn)
  }
}
