package com.strong_links.core

/**
 * Helper object to deal with errors
 */
object Errors {

  class SystemException(val params: Seq[LoggingParameter], val cause: Option[Throwable])
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
    throwError(LoggingParameter.concat(params, moreParams), None)

  def fatal(cause: Throwable, params: LoggingParameter*) =
    throwError(params, Some(cause))

  def fatal(cause: Throwable, params: Seq[LoggingParameter], moreParams: Seq[LoggingParameter]) =
    throwError(LoggingParameter.concat(params, moreParams), Some(cause))

  def trap[R](params: LoggingParameter*)(anyCode: => R): R =
    try anyCode catch {
      case e => throwError(params, Some(e))
    }

  private def _liveTrap[R](params: (() => LoggingParameter)*)(anyCode: => R): R =
    try anyCode catch {
      case e => throwError(params.map(_()), Some(e))
    }

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

  private def exceptionChain(e: Throwable): List[Throwable] =
    if (e == null) Nil else exceptionChain(e.getCause) :+ e

  private def getParams(t: Throwable, level: String = ""): Seq[LoggingParameter] =
    exceptionChain(t).flatMap(_ match {
      case e: SystemException =>
        e.params
      case e: scala.collection.parallel.CompositeThrowable =>
        val buf = scala.collection.mutable.ListBuffer[LoggingParameter]()
        buf += new StringLoggingParameter("Multiple exceptions start")
        for ((t, i) <- e.throwables.toList.zipWithIndex) {
          val iStr = i.toString
          val newLevel = level match { case "" => iStr; case x => x + "." + iStr }
          buf += new StringLoggingParameter("Exception #" + newLevel)
          buf ++= getParams(t, newLevel)
        }
        buf += new StringLoggingParameter("Multiple exceptions end")
        buf.toList
      case e =>
        Seq(new StringLoggingParameter(OS.getFinalClassName(e) + ": " + OS.getExceptionMessage(e)))
    })

  private def getStackTrace(t: Throwable, level: String = ""): Seq[String] = {
    def fmt(t: Throwable) = (OS.getFinalClassName(t) + " exception: " + OS.getExceptionMessage(t)) +:
      t.getStackTrace.map("    at " + _.toString.trim).toSeq
    exceptionChain(t).flatMap(_ match {
      case e: scala.collection.parallel.CompositeThrowable =>
        val buf = scala.collection.mutable.ListBuffer[String]()
        buf += ("Multiple exceptions start")
        for ((t, i) <- e.throwables.toList.zipWithIndex) {
          val iStr = i.toString
          val newLevel = level match { case "" => iStr; case x => x + "." + iStr }
          buf += ("Exception #" + newLevel)
          buf ++= getStackTrace(t, newLevel)
        }
        buf += ("Multiple exceptions end")
        buf.toList
      case e =>
        fmt(e)
    })
  }

  private def stackTrace(t: Throwable) = getStackTrace(t).mkString("\n", "\n", "")

  def formatException(t: Throwable, withStackTrace: Boolean) =
    LoggingParameter.safeFormat(getParams(t)) + (if (withStackTrace) stackTrace(t) else "")
}
