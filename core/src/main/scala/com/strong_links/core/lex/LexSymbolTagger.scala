package com.strong_links.core.lex

import com.strong_links.core._
import java.lang.reflect.Method

trait LexSymbolTagger {

  import scala.collection.mutable._

  // LexSymbol fields in the class are actually defined as getter methods at the JVM level.
  // Build a set of these methods. This set will gradually evolve: entries will be removed from
  // the set as they are added to the map enabling the translation from the LexSymbol object to
  // its label.
  private val methods = (Set[Method]() /: getClass.getMethods.filter(m =>
    classOf[LexSymbol].isAssignableFrom(m.getReturnType) && !ignoredMethod(m)))(_ += _)

  private val symbolToLabelMap = Map[LexSymbol, String]()

  private def updateMap {
    for (
      m <- methods.toList;
      i = m.invoke(this) if i != null; // When null, the val field has not been initialized yet!
      s = i.asInstanceOf[LexSymbol] // Safe to access it now.
    ) {
      symbolToLabelMap += (s -> m.getName) // Add name to map.
      methods -= m // Remove this method from the set as we fixed it.
    }
  }

  def mapper(s: LexSymbol) = {
    def get(recover: Boolean): String = symbolToLabelMap.get(s) match {
      case Some(x) => x
      case None if recover => updateMap; get(false)
      case _ => Errors.fatal("Key not found.")
    }
    get(true)
  }

  // Set of known identifier symbols, and a method to get them as an ordered array.
  private val idValues = Set[LexIdentifierSymbol]()

  def getIdentifierSymbols = idValues.toList.sortWith(_.identifier < _.identifier).toArray

  // Set of known special symbols, and a method to get them as a list ordered by decreasing
  // lengths, as we must match longer special symbols first.
  private val specialValues = Set[LexSpecialSymbol]()

  def getSpecialSymbols = specialValues.toList.sortWith(_.special.length > _.special.length)

  private def ignoredMethod(m: Method) = {
    val name = m.getName
    name == "symbol" || name == "idSymbol" || name == "specialSymbol"
  }

  def /* Ignored above */ symbol = new LexSymbol(mapper)

  def /* Ignored above */ idSymbol = {
    val s = new LexIdentifierSymbol(None, mapper)
    idValues += s
    s
  }

  def /* Ignored above */ idSymbol(identifier: String) = {
    val s = new LexIdentifierSymbol(Some(identifier), mapper)
    idValues += s
    s
  }

  def /* Ignored above */ specialSymbol(k: String) = {
    val s = new LexSpecialSymbol(k, mapper)
    specialValues += s
    s
  }
}
