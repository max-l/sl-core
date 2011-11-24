package net.strong_links.core

import net.strong_links.core._

case class SystemException(msg: String) extends Exception(msg)

case class UserException(msg: I18n) extends Exception("User exception; please handle msg...")

//case class UserException (msg: I18n) extends Exception

/**
 * Helper object to deal with errors
 */
object Errors {
  /**
   * Signal a general fatal error .
   */
  private def throwError(params: List[ErrorParameter]): Nothing = {
    throw new SystemException(params.map(_.format).mkString("; "))
  }
  
  def fatal(params: ErrorParameter*): Nothing = {
    val list = params.toList
    if (list.isEmpty)
      throwError(List("No error parameters supplied." : ErrorParameter))
    else
      throwError(list)
  }

  def warning(params: ErrorParameter*) {
    Console.err.println("Warning: " + params.map(_.format).mkString("; "))
  }

  /**
   * Signal a bad value error, for classes.
   */
  def badValue[T <: AnyRef](value: T)(implicit d: DummyParam) =
    fatal("Bad value _ for _." << (value, value.getClass.getCanonicalName))

  /**
   * Signal a bad value error, for basic value types..
   */
  def badValue[T <: AnyVal](value: T)(implicit d1: DummyParam, d2: DummyParam) =
    fatal("Bad value _." << value)

  /** 
   * For "Not Implemented" methods.  
   */
  def notImplemented = {
    fatal("Not implemented.") 
  }
  
  def user(msg: I18n): Nothing = {
    throw new UserException(msg)
  }
}

trait ErrorParameter {
  def format: String  
}

class StringErrorParameter(s: String) extends ErrorParameter {
  def format = s
}

class PluggedStringErrorParameter(ps: PluggedString) extends ErrorParameter {
  def format = ps.format(failsafe = true, quoted = true)
}

class ExceptionErrorParameter(e: Exception) extends ErrorParameter {
  def format = ("Catched exception: _" << e.getMessage).format(failsafe = true, quoted = true)
}
