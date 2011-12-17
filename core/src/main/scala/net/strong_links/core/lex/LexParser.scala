package net.strong_links.core.lex

import net.strong_links.core._

abstract class LexParser(pData: String) extends LexSymbolTagger with Logging {

  val Other, Eof, Comma, Dot, Colon, LeftParenthesis, RightParenthesis = Value
  val Identifier, CharacterString, Number = Value

  protected val ETX = '\u0000'
  private var lineNumber = 1
  protected var pos, startPos = 0
  protected var token = new LexToken(Other, Other.toString, lineNumber, pos)
  protected val data = pData.replace("\r\n", "\n") + ETX
  protected var currentChar = data(pos)

  // Token creators
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
      Errors.fatal("Invalid token _; expected one of _." <<< (token, validChoices))
  }

  // Some rules.
  protected def isIdentifierFirstCharacter: Boolean = {
    currentChar.isLetter || currentChar == '_' || currentChar == '$'
  }

  protected def isIdentifierCharacter: Boolean = {
    isIdentifierFirstCharacter || currentChar.isDigit
  }

  // Methods to get the various symbols.
  def getWord(word: String) {
    setToken(Identifier, word)
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

  protected def getString {
    val start = pos + 1
    move
    def unterminated = Errors.fatal("Unterminated character string.")
    while (currentChar != ETX && currentChar != '"' && currentChar != '\n') {
      if (currentChar == '\\') {
        move
        if (currentChar == ETX)
          unterminated
        move
      } else
        move
    }
    if (currentChar != '"')
      unterminated
    setToken(CharacterString, data.substring(start, pos))
    move
  }

  def getLineComments(s: LexSymbol) {
    val c = eatUntil(currentChar == '\n')
    if (currentChar == '\n')
      move
    setToken(s, c)
  }

  def oneChar(symbol: LexSymbol) { oneChar(symbol, currentChar.toString) }

  def oneChar(symbol: LexSymbol, value: String) { setToken(symbol, value); move }

  def twoChar(symbol: LexSymbol, value: String) { setToken(symbol, value); move(2) }

  def getMiscellaneous {
    currentChar match {
      case ETX => move; setToken(Eof, "(EOF")
      case ',' => oneChar(Comma)
      case '(' => oneChar(LeftParenthesis)
      case ')' => oneChar(RightParenthesis)
      case '.' => oneChar(Dot)
      case ':' => oneChar(Colon)
      case _ => oneChar(Other)
    }
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
    if (isIdentifierFirstCharacter) {
      val sb = new StringBuilder
      while (isIdentifierCharacter) {
        sb.append(currentChar)
        move
      }
      getWord(sb.toString)
    } else if (currentChar.isDigit)
      getNumber
    else
      getMiscellaneous
  }
}

