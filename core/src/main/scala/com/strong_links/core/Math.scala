package com.strong_links.core

object Math {

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
}