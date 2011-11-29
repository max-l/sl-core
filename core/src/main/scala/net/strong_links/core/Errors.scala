package net.strong_links.core

class SystemException(msg: String) extends Exception(msg)

class UserException(val msg: I18n) extends Exception("User exception")

/**
 * Helper object to deal with errors
 */
object Errors {
  /**
   * Signal a general fatal error .
   */
  private def throwError(params: LoggingParameter*): Nothing = {
    throw new SystemException(LoggingParameter.format(params: _*))
  }
  
  def fatal(params: LoggingParameter*): Nothing = {
    if (params.isEmpty)
      throwError("No logging parameters supplied.")
    else
      throwError(params: _*)
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

