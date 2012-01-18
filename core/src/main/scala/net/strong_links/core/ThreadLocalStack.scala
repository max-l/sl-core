package net.strong_links.core

class ThreadLocalStack[T] {

  private val value = new ThreadLocal[T]

  def using[R](newValue: T)(code: => R): R = {
    val previousValue = value.get
    value.set(newValue)
    val result = code
    value.set(previousValue)
    result
  }

  def isDefined = value.get != null

  implicit def get = {
    if (value.get == null)
      Errors.fatal("Method 'using' has not been called; class is _." << this.getClass.getCanonicalName)
    value.get
  }

  def getOrElse(default: T) = {
    val x = value.get
    if (x == null)
      default
    else
      x
  }
}