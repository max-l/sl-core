package com.strong_links.core.lex

import com.strong_links.core._

class LexToken(val symbol: LexSymbol, val value: String, val lineNumber: Int,
               val startPos: Int, val endPos: Int, val startPosWithWhiteSpace: Int) {

  override def toString = "_ (_)" << (symbol.label, value)

  def is(s: LexSymbol) = s == symbol

  def isNot(s: LexSymbol) = s != symbol

  def in(seq: LexSymbol*) = seq.contains(symbol)

  def notIn(seq: LexSymbol*) = !seq.contains(symbol)
}

