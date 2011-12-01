package net.strong_links.core

class LexError extends Exception

// Here are the various symbols/operators needed to process Scala files, Po files, and 
// 'C' style expressions computing the plural form.
object LexSymbol extends Enumeration {
  type LexSymbol = Value
  val eof, identifier, string, verbatimString, scalaLineComments, poLineComments, blockComments, number, leftParenthesis, rightParenthesis, comma, msgid, msgctxt, msgid_plural, msgstr, leftBracket, rightBracket, other, n, plus, minus, logicalNot, multiply, divide, modulo, lessThan, lessThanOrEqual, greaterThan, greaterThanOrEqual, equal, notEqual, logicalAnd, logicalOr, questionMark, colon, unaryMinus, triadic, htmlStartComment, htmlEndComment, template, templateEnd, dot, preserveSpaces = Value

  implicit def SymbolToSymbolHelper(symbol: LexSymbol): LexSymbolHelper = {
    new LexSymbolHelper(symbol)
  }
}

import LexSymbol._

class LexSymbolHelper(symbol: LexSymbol) {
  def in(set: Set[LexSymbol]) = {
    set.contains(symbol)
  }

  def notIn(set: Set[LexSymbol]) = {
    !in(set)
  }
}

class BasicLexParser(pData: String, logger: Xlogger) {
  protected val ETX = '\u2403'
  protected var lineNumber = 1
  protected val data = pData.replace("\r\n", "\n") + ETX
  protected var pos, startPos = 0
  protected var currentChar = data(pos)
  protected val sb = new StringBuilder
  protected var startLineNumber = 1
  protected var symbol = LexSymbol.other
  protected var symbolValue = ""

  // This method can be overriden to provide the source file name, if any.
  protected def getFileName: Option[String] = None

  // Error/warning handling
  protected def addSourceInfo(startLineNumber: Int, msg: LoggingParameter) = {
    val loggingParameters: Seq[LoggingParameter] = getFileName match {
      case Some(fileName) =>
        Seq("At file _, line _" << (fileName, startLineNumber), msg)
      case None =>
        Seq("At line _" << startLineNumber, msg)
    }
    loggingParameters
  }

  protected def error(startLineNumber: Int, msg: LoggingParameter): Nothing = {
    val m = LoggingParameter.safeFormat(addSourceInfo(startLineNumber, msg): _*)
    logger.error(m)
    throw new LexError
  }

  protected def warning(startLineNumber: Int, msg: LoggingParameter) {
    val m = LoggingParameter.format(addSourceInfo(startLineNumber, msg): _*)
    logger.warning(m)
  }

  // Character-level helpers.
  protected def previousChar = {
    if (pos == 0)
      '\n'
    else
      data(pos - 1)
  }

  protected def nextChar = {
    data(pos + 1)
  }

  protected def nextNextChar = {
    data(pos + 2)
  }

  protected def nextNextNextChar = {
    data(pos + 3)
  }

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
  protected def expect(validChoices: Set[LexSymbol]) {
    if (symbol notIn validChoices)
      error(startLineNumber, "Invalid symbol _; expected one of _." <<< (symbol, validChoices.mkString(", ")))
  }

  protected def expect(validChoice: LexSymbol) {
    if (symbol != validChoice)
      error(startLineNumber, "Invalid symbol _; expected _." <<< (symbol, validChoice))
  }

  // Some rules.
  protected def isIdentifierFirstCharacter: Boolean = {
    isIdentifierFirstCharacter(currentChar)
  }

  protected def isIdentifierFirstCharacter(ch: Char): Boolean = {
    ch.isLetter || ch == '_' || ch == '$'
  }

  protected def isIdentifierCharacter: Boolean = {
    isIdentifierCharacter(currentChar)
  }

  protected def isIdentifierCharacter(ch: Char): Boolean = {
    isIdentifierFirstCharacter(ch) || ch.isDigit
  }

  // Methods to get the various symbols.
  private def getIdentifier {
    sb.clear
    while (isIdentifierCharacter) {
      sb.append(currentChar)
      move
    }
    sb.toString match {
      case "n" => symbol = n
      case "msgid" => symbol = msgid
      case "msgctxt" => symbol = msgctxt
      case "msgid_plural" => symbol = msgid_plural
      case "msgstr" => symbol = msgstr
      case "template" => symbol = template
      case "templateEnd" => symbol = templateEnd
      case "preserveSpaces" => symbol = preserveSpaces
      case s => symbol = identifier; symbolValue = s
    }
  }

  // Get a number, and ensure that it can be represented as an Int.
  private def getNumber {
    var v = 0L
    sb.clear
    while (currentChar.isDigit) {
      sb.append(currentChar)
      v = (v * 10) + currentChar.toInt - '0'.toInt
      if (v > Int.MaxValue)
        error(startLineNumber, "Number is too big.")
      move
    }
    symbol = number
    symbolValue = sb.toString
  }

  private def getMiscellaneous {
    val c = currentChar
    move
    symbol = c match {
      case ETX => eof
      case ',' => comma
      case '[' => leftBracket
      case ']' => rightBracket
      case '(' => leftParenthesis
      case ')' => rightParenthesis
      case '+' => plus
      case '-' => minus
      case '*' => multiply
      case '/' => divide
      case '%' => modulo
      case '<' if (currentChar == '=') => move; lessThanOrEqual
      case '<' => lessThan
      case '>' if (currentChar == '=') => move; greaterThanOrEqual
      case '>' => greaterThan
      case '=' if (currentChar == '=') => move; equal
      case '!' if (currentChar == '=') => move; notEqual
      case '!' => logicalNot
      case '&' if (currentChar == '&') => move; logicalAnd
      case '|' if (currentChar == '|') => move; logicalOr
      case '?' => questionMark
      case ':' => colon
      case '.' => dot
      case _ => symbolValue = currentChar.toString; other
    }
  }

  protected def skip(what: LexSymbol) {
    expect(what)
    getSymbol
  }

  protected def eatSymbol = {
    val r = (symbol, symbolValue)
    getSymbol
    r
  }

  def eatString: String = {
    val s = symbolValue
    skip(string)
    s
  }

  def getSymbolHere {
    if (isIdentifierFirstCharacter)
      getIdentifier
    else if (currentChar.isDigit)
      getNumber
    else if (!tryMoreSymbolHere)
      getMiscellaneous
  }

  def tryMoreSymbolHere: Boolean = {
    false
  }

  def getSymbol {
    while (currentChar.isWhitespace)
      move
    startLineNumber = lineNumber
    startPos = pos
    symbolValue = ""
    getSymbolHere
  }
}

class LexParser(pData: String, logger: Xlogger) extends BasicLexParser(pData, logger) {

  private def isVerbatimMarker = {
    currentChar == '"' && nextChar == '"' && nextNextChar == '"'
  }

  private def isScalaLineCommentStart = {
    currentChar == '/' && nextChar == '/'
  }

  private def isPoLineCommentStart = {
    currentChar == '#' && previousChar == '\n'
  }

  private def isBlockCommentEnd = {
    currentChar == '*' && nextChar == '/'
  }

  private def isBlockCommentStart = {
    currentChar == '/' && nextChar == '*'
  }

  private def getVerbatimString {
    def escape(c: Char) = {
      c match {
        case '\n' => "\\n"
        case '\'' => "\\'"
        case '"' => "\\\""
        case '\\' => "\\\\"
        case _ => c.toString
      }
    }
    move(3)
    val s = eatUntil(isVerbatimMarker)
    move(3)
    // Exception in verbatim strings: characters such as \n and \t and
    // taken "as is" (two distinct characters), but the escape sequence
    // \u0000 is actually understood. So we need to do a bit of gymnastic to
    // deal with this.
    val t = s.replace("\\\\", "\uFFFF").
      replace("\\u", "\uFFFE").
      replace("\uFFFF", "\\\\").
      map(escape).
      mkString.
      replace("\uFFFE", "\\u")
    symbol = verbatimString
    symbolValue = t
  }

  private def getString {
    val start = pos + 1
    move
    while (currentChar != ETX && currentChar != '"' && currentChar != '\n') {
      if (currentChar == '\\') {
        move
        if (currentChar == ETX)
          error(lineNumber, "Unterminated character string.")
        move
      } else
        move
    }
    if (currentChar != '"')
      error(lineNumber, "Unterminated character string.")
    symbol = string
    symbolValue = data.substring(start, pos)
    move
  }

  private def getLineComments(s: LexSymbol) {
    val c = eatUntil(currentChar == '\n')
    if (currentChar == '\n')
      move
    symbol = s
    symbolValue = c
  }

  private def getBlockComments {
    val start = pos
    move(2)
    var level = 1
    do {
      eatUntil(isBlockCommentStart || isBlockCommentEnd)
      if (isBlockCommentStart)
        level += 1
      else
        level -= 1
      move(2)
    } while (level != 0)
    symbol = blockComments
    symbolValue = data.substring(start, pos)
  }

  override def tryMoreSymbolHere: Boolean = {
    if (isVerbatimMarker)
      getVerbatimString
    else if (currentChar == '"')
      getString
    else if (isScalaLineCommentStart)
      getLineComments(scalaLineComments)
    else if (isPoLineCommentStart)
      getLineComments(poLineComments)
    else if (isBlockCommentStart)
      getBlockComments
    else if (currentChar.isDigit)
      super.getSymbolHere
    else
      return false

    return true
  }
}

object Lex {
  def normalizeName(name: String, context: => LoggingParameter) = {
    def cleanUnderscores(s: String): String = if (!s.contains("__")) s else cleanUnderscores(s.replace("__", "_"))
    val x = cleanUnderscores(Convert.generic(name.toLowerCase, None) {
      case c if c.isLetterOrDigit => null
      case _ => "_"
    })
    if (x.length == 0)
      Errors.fatal("Can't generate a normalized name for _" << name, context)
    if (x(0).isLetter)
      x
    else
      "_" + x
  }
}