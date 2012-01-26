package com.strong_links.core.lex

import com.strong_links.core._

object LexParser {

  private def patch(s: String, from: String, to: String) = {
    val mickeyMouse = "\uFFFF\u1234\uFFFF"
    val doubleBackSlash = "\\\\"
    val x = s.replace(doubleBackSlash, mickeyMouse)
    if (x.contains(from))
      x.replace(from, to).replace(mickeyMouse, doubleBackSlash)
    else
      s
  }

  def toRealLineFeeds(s: String) = patch(s, "\\n", "\n")

  def toFalseLineFeeds(s: String) = patch(s, "\n", "\\n")
}

abstract class LexParser(pData: String) extends LexSymbolTagger with Logging {

  val Other, Eof, Identifier, CharacterString, Number = symbol
  val Comma = specialSymbol(",")
  val Dot = specialSymbol(".")
  val Colon = specialSymbol(":")
  val LeftParenthesis = specialSymbol("(")
  val RightParenthesis = specialSymbol(")")

  protected val ETX = '\u0000'
  private var lineNumber = 1
  protected var pos, startPos = 0
  protected var token = defaultToken
  protected val data = pData.replace("\r\n", "\n") + ETX
  protected var currentChar = data(pos)

  // Token creators
  def defaultToken = new LexToken(Other, Other.toString, lineNumber, pos)
  def setToken(symbol: LexSymbol, value: String) { token = new LexToken(symbol, value, lineNumber, startPos) }
  def setToken(symbol: LexSymbol) { setToken(symbol, symbol.toString) }

  // Character-level helpers.
  protected def previousChar = if (pos == 0) '\n' else data(pos - 1)

  protected def nextChar = data(pos + 1)

  protected def nextNextChar = data(pos + 2)

  protected def nextNextNextChar = data(pos + 3)

  protected def move {
    if (currentChar != ETX) {
      if (currentChar == '\n')
        lineNumber += 1
      pos += 1
      currentChar = data(pos)
    }
  }

  protected def move(n: Int) {
    for (i <- 0 until n)
      move
  }

  protected def eatUntil(c: => Boolean) = {
    val start = pos
    while (currentChar != ETX && !c)
      move
    data.substring(start, pos)
  }

  // Symbol checks.
  protected def expect(validChoices: LexSymbol*) {
    if (token notIn (validChoices: _*))
      if (validChoices.length == 1)
        Errors.fatal("Invalid token _; expected _." <<< (token, validChoices.toSeq(0)))
      else
        Errors.fatal("Invalid token _; expected one of _." <<< (token, validChoices))
  }

  // Some rules.
  protected def isIdentifierFirstCharacter: Boolean = {
    currentChar.isLetter || currentChar == '_' || currentChar == '$'
  }

  protected def isIdentifierCharacter: Boolean = {
    isIdentifierFirstCharacter || currentChar.isDigit
  }

  lazy val identifierSymbols = getIdentifierSymbols

  def getWord(word: String) {
    def search(value: String, low: Int, high: Int): Option[LexIdentifierSymbol] =
      if (high < low)
        None
      else {
        val mid = (low + high) / 2
        val k = identifierSymbols(mid).identifier
        if (k > value)
          search(value, low, mid - 1)
        else if (k < value)
          search(value, mid + 1, high)
        else
          Some(identifierSymbols(mid))
      }
    search(word, 0, identifierSymbols.length - 1) match {
      case None => setToken(Identifier, word)
      case Some(s) => setToken(s, s.identifier)
    }
  }

  // Get a number, and ensure that it can be represented as an Int.
  private def getNumber {
    var v = 0L
    val sb = new StringBuilder
    while (currentChar.isDigit) {
      sb.append(currentChar)
      v = (v * 10) + currentChar.toInt - '0'.toInt
      if (v > Int.MaxValue)
        Errors.fatal("Number is too big.")
      move
    }
    setToken(Number, sb.toString)
  }

  protected def getQuoted(quote: Char, allowEscape: Boolean, symbol: LexSymbol) {
    // Skip over the intial quote
    move
    val start = pos
    def unterminated = Errors.fatal("Unterminated _." << symbol)
    while (currentChar != ETX && currentChar != quote && currentChar != '\n') {
      if (allowEscape && (currentChar == '\\')) {
        move
        if (currentChar == ETX)
          unterminated
        move
      } else
        move
    }
    if (currentChar != quote)
      unterminated
    setToken(symbol, data.substring(start, pos))
    move
  }

  protected def getString = getQuoted('"', true, CharacterString)

  protected def getTickedIdentifier = getQuoted('`', false, Identifier)

  def getLineComments(s: LexSymbol) {
    val c = eatUntil(currentChar == '\n')
    if (currentChar == '\n')
      move
    setToken(s, c)
  }

  lazy val specialSymbols = getSpecialSymbols

  def getMiscellaneous {
    for (ss <- specialSymbols) {
      val k = ss.special
      val L = k.length
      var i = 0
      var p = pos
      while (i < L && data(p) == k(i)) {
        p += 1
        i += 1
      }
      if (i == L) {
        setToken(ss, k)
        move(L)
        // Yes, this is a bit cowboy, but faster.
        return
      }
    }
    setToken(Other, currentChar.toString)
    move(1)
  }

  protected def skip(what: LexSymbol) {
    expect(what)
    getToken
  }

  protected def eatAnyToken = {
    val t = token
    getToken
    t
  }

  def eatToken(symbol: LexSymbol) = {
    expect(symbol)
    eatAnyToken
  }

  def getToken {
    while (currentChar.isWhitespace)
      move
    startPos = pos
    if (currentChar == ETX)
      setToken(Eof, "End of file")
    else if (isIdentifierFirstCharacter) {
      val sb = new StringBuilder
      while (isIdentifierCharacter) {
        sb.append(currentChar)
        move
      }
      getWord(sb.toString)
    } else if (currentChar.isDigit)
      getNumber
    else if (currentChar == '`')
      getTickedIdentifier
    else
      getMiscellaneous
  }
}

