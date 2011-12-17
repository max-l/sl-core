package net.strong_links.core.lex

import net.strong_links.core._

class LexToken(val symbol: LexSymbol, val value: String, val lineNumber: Int, val pos: Int) {

  override def toString = {
    val label = symbol.toString
    if (label == value) label else ("_ (_)" << (label, value))
  }

  def is(s: LexSymbol) = s == symbol

  def isNot(s: LexSymbol) = s != symbol

  def in(set: LexSymbol*) = set.contains(symbol)

  def notIn(set: LexSymbol*) = !set.contains(symbol)
}

