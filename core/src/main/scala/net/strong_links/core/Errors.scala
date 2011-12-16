package net.strong_links.core

/**
 * Helper object to deal with errors
 */
object Errors {

  class SystemException(params: Seq[LoggingParameter], cause: Option[Throwable])
    extends Exception("System exception", cause.getOrElse(null)) {
    override def getMessage() = LoggingParameter.safeFormat(params)
  }

  private def throwError(params: Seq[LoggingParameter], cause: Option[Throwable]): Nothing = {
    val p = if (params.length == 0)
      Seq("(No logging parameters supplied)": LoggingParameter)
    else
      params
    throw new SystemException(p, cause)
  }

  def fatal(params: LoggingParameter*) =
    throwError(params, None)

  def fatal(params: Seq[LoggingParameter], moreParams: Seq[LoggingParameter]) =
    throwError(params.toList ::: moreParams.toList, None)

  def fatal(cause: Throwable, params: LoggingParameter*) =
    throwError(params, Some(cause))

  def fatal(cause: Throwable, params: Seq[LoggingParameter], moreParams: Seq[LoggingParameter]) =
    throwError(params.toList ::: moreParams.toList, Some(cause))

  def trap[R](params: LoggingParameter*)(anyCode: => R): R =
    try anyCode catch { case e => throwError(params, Some(e)) }

  private def _liveTrap[R](params: (() => LoggingParameter)*)(anyCode: => R): R =
    try anyCode catch { case e => throwError(params.map(_()), Some(e)) }

  def liveTrap[R](p1: => LoggingParameter)(anyCode: => R): R =
    _liveTrap(p1 _)(anyCode)

  def liveTrap[R](p1: => LoggingParameter, p2: => LoggingParameter)(anyCode: => R): R =
    _liveTrap(p1 _, p2 _)(anyCode)

  def liveTrap[R](p1: => LoggingParameter, p2: => LoggingParameter, p3: => LoggingParameter)(anyCode: => R): R =
    _liveTrap(p1 _, p2 _, p3 _)(anyCode)

  def liveTrap[R](p1: => LoggingParameter, p2: => LoggingParameter, p3: => LoggingParameter, p4: => LoggingParameter)(anyCode: => R): R =
    _liveTrap(p1 _, p2 _, p3 _, p4 _)(anyCode)

  def liveTrap[R](p1: => LoggingParameter, p2: => LoggingParameter, p3: => LoggingParameter, p4: => LoggingParameter, p5: => LoggingParameter)(anyCode: => R): R =
    _liveTrap(p1 _, p2 _, p3 _, p4 _, p5 _)(anyCode)

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

  def exceptionChain(e: Throwable): List[Throwable] =
    if (e == null) Nil else e +: exceptionChain(e.getCause)

  def formatException(t: Throwable, withStackTrace: Boolean) = {
    val cs = new CharStream
    val chain = exceptionChain(t)
    val last = chain.reverse.head
    for (e <- chain) {
      cs.println("_ exception: _" << (OS.getFinalClassName(e), OS.getExceptionMessage(e)))
      if (withStackTrace && (e eq last))
        for (ste <- e.getStackTrace)
          cs.println("    at " + ste.toString.trim)
    }
    cs.close
  }
}
