package net.strong_links.core.lex

import net.strong_links.core._
import java.lang.reflect.Method

trait LexSymbolTagger {

  import scala.collection.mutable._

  // LexSymbol fields in the class are actually defined as getter methods at the JVM level.
  // Build a set of these methods. This set will gradually evolve: entries will be removed from
  // the set as they are added to the map enabling the translation from the LexSymbol object to
  // its label.
  private val methods = (Set[Method]() /: getClass.getMethods.filter(m =>
    m.getReturnType == classOf[LexSymbol] && m.getName != "Value"))(_ += _)

  private val map = Map[LexSymbol, String]()

  private def updateMap {
    for (
      m <- methods.toList;
      i = m.invoke(this) if i != null; // When null, the val field has not been initialized yet!
      s = i.asInstanceOf[LexSymbol] // Safe to access it now.
    ) {
      map += (s -> m.getName) // Add name to map.
      methods -= m // Remove this method from the set.
    }
  }

  def Value = new LexSymbol(s => { if (!map.contains(s)) updateMap; map(s) })
}
