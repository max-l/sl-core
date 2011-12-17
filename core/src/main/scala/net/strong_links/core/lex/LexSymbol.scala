package net.strong_links.core.lex

class LexSymbol(map: LexSymbol => String) {

  override def toString = map(this)
}

