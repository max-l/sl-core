package net.strong_links.core

import java.lang.StackTraceElement

class StrongLinksException(s: String) extends Exception(s)

class SystemException(msg: String, val causedByStackTrace: Option[Array[StackTraceElement]]) extends StrongLinksException(msg)

object SystemException {
  def unapply(se: SystemException): Option[Array[StackTraceElement]] = {
    se.causedByStackTrace
  }
}

class UserException(val msg: I18n) extends StrongLinksException("User exception")

/**
 * Helper object to deal with errors
 */
object Errors {
  /**
   * Signal a general fatal error .
   */
  private def throwError(params: LoggingParameter*): Nothing = {
    val message = LoggingParameter.format(params: _*)
    // Try to see if a root cause exception is supplied in the parameters. By convention,
    // the root cause exception is located at the very "far right" of the argument list, but not
    // it is not necessarily the last argument.
    val lastStackTrace = params.collect { case elp: ExceptionLoggingParameter => elp.stackTrace }.reverse.headOption
    throw new SystemException(message, lastStackTrace)
  }

  def fatal(params: LoggingParameter*): Nothing = {
    if (params.isEmpty)
      throwError("No logging parameters supplied.")
    else
      throwError(params: _*)
  }

  def trap[R](params: LoggingParameter*)(anyCode: => R): R = {
    recover(anyCode) using { e =>
      Errors.fatal(params.toList :+ (e: LoggingParameter): _*)
    }
  }

  case class Recoverable[R](results: Option[R], e: Option[Exception]) {
    def using(recoverCode: Exception => R) = (results, e) match {
      case (Some(r), None) => r
      case (None, Some(e)) => recoverCode(e)
      case _ => Errors.fatal("Invalid pattern.")
    }
  }

  def recover[R](anyCode: => R): Recoverable[R] = {
    try Recoverable(Some(anyCode), None) catch {
      case e: Exception =>
        Recoverable(None, Some(e))
    }
  }

  def trapByName[R](p1: => LoggingParameter)(anyCode: => R): R =
    _trapByName(p1 _)(anyCode)

  def trapByName[R](p1: => LoggingParameter, p2: => LoggingParameter)(anyCode: => R): R =
    _trapByName(p1 _, p2 _)(anyCode)

  def trapByName[R](p1: => LoggingParameter, p2: => LoggingParameter, p3: => LoggingParameter)(anyCode: => R): R =
    _trapByName(p1 _, p2 _, p3 _)(anyCode)

  def trapByName[R](p1: => LoggingParameter, p2: => LoggingParameter, p3: => LoggingParameter, p4: => LoggingParameter)(anyCode: => R): R =
    _trapByName(p1 _, p2 _, p3 _, p4 _)(anyCode)

  def trapByName[R](p1: => LoggingParameter, p2: => LoggingParameter, p3: => LoggingParameter, p4: => LoggingParameter, p5: => LoggingParameter)(anyCode: => R): R =
    _trapByName(p1 _, p2 _, p3 _, p4 _, p5 _)(anyCode)

  private def _trapByName[R](params: (() => LoggingParameter)*)(anyCode: => R): R = {
    recover(anyCode) using { e =>
      Errors.fatal((params.toList.map(_()) :+ (e: LoggingParameter)): _*)
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

  def !!![R](anyCode: => R) = {
    { case _ => anyCode }: PartialFunction[Exception, R]
  }
}

