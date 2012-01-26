package com.strong_links.core

object Convert {

  def generic(s: String, quote: Option[Char] = None)(map: PartialFunction[Char, String]) = {
    var i = 0
    val n = s.length
    var found = false
    while (i < n && !found) {
      val ch = s(i)
      found = map(ch) != null
      i += 1
    }
    val r = 
      if (found) {
        i -= 1
        val sofar = if (i == 0) "" else s.substring(0, i)
        val sb = new StringBuilder(sofar)
        while (i < n) {
          val ch = s(i)
          val x = map (ch)
          if (x == null)
            sb.append(ch)
          else
            sb.append(x)
          i += 1
        }
        sb.toString
      } else
        s
    quote match {
      case None => r
      case Some(q) => q + r + q
    }
  }

  def toHtml(s: String, quote: Boolean = false) = generic(s, if (quote) Some('"') else None) {
    case '<' => "&lt;"
    case '>' => "&gt;" 
    case '&' => "&amp;" 
    case '"' => "&#34;"        // Some HTML 3.2 browsers do not understand &quot; 
    case '\u00A0' => "&nbsp;"
    case c if c.isControl => c.toInt.formatted("&#%d;") 
    case _ => null 
  }
  
  // From http://www.w3schools.com/js/js_special_characters.asp
  def toJs(s: String, quote: Boolean = false) = generic(s, if (quote) Some('\'') else None) {
    case '\'' => "\\'" 
    case '\"' => "\\\"" 
    case '\\' => "\\\\" 
    case '\n' => "\\n" 
    case '\r' => "\\r" 
    case '\t' => "\\t" 
    case '\b' => "\\b" 
    case '\f' => "\\f" 
    case c if c.isControl => c.toInt.formatted("\\u%04X") 
    case _ => null 
  }
  
  // From Odersky, Martin; Programming in Scala, 2nd Edition; page 78 
  def toScala(s: String, quote: Boolean = false) = generic(s, if (quote) Some('"') else None) {
    case '\n' => "\\n" 
    case '\b' => "\\b" 
    case '\t' => "\\t" 
    case '\f' => "\\f" 
    case '\r' => "\\r" 
    case '"' => "\\\"" 
    case '\'' => "\\'" 
    case '\\' => "\\\\" 
    case c if c.isControl => c.toInt.formatted("\\u%04X") 
    case _ => null 
  }
}
