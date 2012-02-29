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

  val Undefined, Other, Eof, Identifier, CharacterString, Number = symbol
  val Comma = specialSymbol(",")
  val Dot = specialSymbol(".")
  val Colon = specialSymbol(":")
  val LeftParenthesis = specialSymbol("(")
  val RightParenthesis = specialSymbol(")")
  val BlockComments = symbol

  val ETX = '\u0000'

  private val data = pData.replace("\r\n", "\n") + ETX
  private var lineNumber = 1
  private var pos, startPos, startPosWithWhiteSpace = 0
  private var _currentChar = data(pos)

  private var _token = new LexToken(Undefined, "", lineNumber, pos, pos, startPosWithWhiteSpace)

  def currentChar = _currentChar
  def token = _token

  // Token creators
  def setToken(symbol: LexSymbol, value: String) { _token = new LexToken(symbol, value, lineNumber, startPos, pos, startPosWithWhiteSpace) }
  def setToken(symbol: LexSymbol) { setToken(symbol, "") }

  // Get data between two tokens, included or excluded.
  def getDataBetween(startToken: LexToken, includeStartToken: Boolean,
                     endToken: LexToken, includeEndToken: Boolean, includeWhiteSpace: Boolean) = {
    val start = if (includeStartToken)
      if (includeWhiteSpace) startToken.startPosWithWhiteSpace else startToken.startPos
    else
      startToken.endPos
    val end = if (includeEndToken) endToken.endPos else endToken.startPos
    data.substring(start, end)
  }

  // Character-level helpers.
  def previousChar = if (pos == 0) '\n' else data(pos - 1)

  def nextChar = data(pos + 1)

  def nextNextChar = data(pos + 2)

  def nextNextNextChar = data(pos + 3)

  def move {
    if (currentChar != ETX) {
      if (currentChar == '\n')
        lineNumber += 1
      pos += 1
      _currentChar = data(pos)
    }
  }

  def move(n: Int) {
    for (i <- 0 until n)
      move
  }

  def eatUntil(c: => Boolean) = {
    val start = pos
    while (currentChar != ETX && !c)
      move
    if (currentChar == ETX)
      Errors.fatal("Unexpected end of file.")
    data.substring(start, pos)
  }

  // Symbol checks.
  def expect(validChoice: LexSymbol) {
    if (token isNot validChoice)
      Errors.fatal("Invalid token _; expected _." <<< (token, validChoice))
  }

  def expect(validChoices: LexSymbol*) {
    if (token notIn (validChoices: _*))
      Errors.fatal("Invalid token _; expected one of _." <<< (token, validChoices))
  }

  def findToken(symbol: LexSymbol) {
    while (token notIn (symbol, Eof))
      getToken
    expect(symbol)
  }

  // Some rules.
  def isIdentifierFirstCharacter: Boolean = {
    currentChar.isLetter || currentChar == '_' || currentChar == '$'
  }

  def isIdentifierCharacter: Boolean = {
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
      case None    => setToken(Identifier, word)
      case Some(s) => setToken(s, s.identifier)
    }
  }

  // Get a number, and ensure that it can be represented as an Int.
  def getNumber {
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

  def getQuoted(quote: Char, allowEscape: Boolean, symbol: LexSymbol) {
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
    move
    setToken(symbol, data.substring(start, pos - 1))
  }

  def getString = getQuoted('"', true, CharacterString)

  def getTickedIdentifier = getQuoted('`', false, Identifier)

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
        move(L)
        setToken(ss, k)
        // Yes, this is a bit cowboy, but faster.
        return
      }
    }
    move(1)
    setToken(Other, currentChar.toString)
  }

  def skip(what: LexSymbol) {
    expect(what)
    getToken
  }

  def skipped(what: LexSymbol) = { val x = token is what; if (x) skip(what); x }

  def eatAnyToken = {
    val t = token
    getToken
    t
  }

  def eatToken(symbols: LexSymbol*) = {
    expect(symbols: _*)
    eatAnyToken
  }

  private var eofAnnounced = false

  def getToken {
    startPosWithWhiteSpace = pos
    while (currentChar.isWhitespace)
      move
    startPos = pos
    if (currentChar == ETX) {
      if (eofAnnounced)
        Errors.fatal("Reading past end of file.")
      eofAnnounced = true
      setToken(Eof, "End of file")
    } else if (isIdentifierFirstCharacter) {
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

  def isBlockCommentStart = currentChar == '/' && nextChar == '*'

  def isBlockCommentEnd = currentChar == '*' && nextChar == '/'

  def getBlockComments {
    val start = pos
    move(2)
    var level = 1
    do {
      eatUntil(isBlockCommentStart || isBlockCommentEnd)
      if (isBlockCommentStart)
        level += 1
      else if (isBlockCommentEnd)
        level -= 1
      else
        Errors.fatal("Unclosed block comment.")
      move(2)
    } while (level != 0)
    setToken(BlockComments, data.substring(start, pos))
  }
}

