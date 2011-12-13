package net.strong_links.core

class StrongLinksException(s: String) extends Exception(s)

class SystemException(msg: String) extends StrongLinksException(msg)

class UserException(val msg: I18n) extends StrongLinksException("User exception")

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

  def context[R](params: LoggingParameter*)(anyCode: => R): R = {
    def mkParams(e: Exception) = params.toList :+ (e: LoggingParameter)
    try anyCode catch {
      case e: Exception =>
        Errors.fatal(mkParams(e): _*)
      case c: Object =>
        val e = new Exception("Unexpected exception having class " + c.getClass.getName)
        Errors.fatal(mkParams(e): _*)
    }
  }

  /**
   * Signal a bad value error, for classes.
   */
  def badValue[T <: AnyRef](value: T)(implicit d0: DummyParam0) =
    fatal("Bad value _ for _." << (value, value.getClass.getCanonicalName))

  /**
   * Signal a bad value error, for basic value types..
   */
  def badValue[T <: AnyVal](value: T)(implicit d1: DummyParam1) =
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

