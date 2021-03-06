package com.strong_links.core.crypto

import com.strong_links.core._

import java.security._
import javax.crypto._
import javax.crypto.spec.SecretKeySpec

trait CryptoUtil {

  import javax.xml.bind.DatatypeConverter._
  
  def bytesToString(a: Array[Byte]) =
    printBase64Binary(a)
    
  def stringToBytes(s: String) =
    s.getBytes("UTF-8")

  def cryptoFieldFromBase64(s: String) = 
    new FullCryptoField(parseBase64Binary(s), s)

  sealed trait CryptoField {
    
    def value: String
    
    def rawValue: Array[Byte]

    def mapValue[A](f: String => A) = f(value)

    lazy val asLong = parseLong(value)

    def matches(f: CryptoField) = {
      val s1 = value
      val s2 = f.value
      val res = s1 == s2

       // lets code defensively :
      if(res && s1.equals(""))
        false
      else
        res
    }
  }

  sealed class FullCryptoField(val rawValue: Array[Byte], val value: String) extends CryptoField
  
  sealed class RawCryptoField(val rawValue: Array[Byte]) extends CryptoField {
    lazy val value = bytesToString(rawValue)
  }

  sealed class RefinedCryptoField(val value: String) extends CryptoField {
    lazy val rawValue = stringToBytes(value)
  }  

  implicit def stringToCryptoField(s: String) = 
    new RefinedCryptoField(s)

  implicit def longToCryptoField(l: Long) =
    new RefinedCryptoField(l.toString)
  
  implicit def byteArrayToCryptoField(a: Array[Byte]) = 
    new RawCryptoField(a)
  
  protected def parseLong(s: String) =
    try {Some(java.lang.Long.parseLong(s))}
    catch {case e:NumberFormatException => None}

  def hmacSha1(args: CryptoField*)(secret: CryptoField) = {

    val algo = createHmacSha1(secret)
    
    for(a <- args) {
      algo.update(a.rawValue)
    }
    algo.doFinal : CryptoField
  }
  
  private def createHmacSha1(secret: CryptoField) = {
    val mac = Mac.getInstance("HmacSHA1")
    mac.init(new SecretKeySpec(secret.rawValue, "HmacSHA1"))
    mac
  }
}
