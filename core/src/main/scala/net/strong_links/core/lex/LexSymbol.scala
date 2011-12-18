package net.strong_links.core.lex

import net.strong_links.core._

class LexSymbol(map: LexSymbol => String) {
  lazy val label = map(this)
  override def toString = label
}

class LexIdentifierSymbol(_identifier: Option[String], map: LexSymbol => String) extends LexSymbol(map) {
  lazy val lowerLabel = label(0).toLower + label.substring(1)
  lazy val identifier = _identifier match {
    case None => lowerLabel
    case Some(x) => x
  }
  override def toString = identifier
}

class LexSpecialSymbol(val special: String, map: LexSymbol => String) extends LexSymbol(map) {
  lazy val betterLabel = label + " (\'" + special + "\')"
  override def toString = betterLabel
}
