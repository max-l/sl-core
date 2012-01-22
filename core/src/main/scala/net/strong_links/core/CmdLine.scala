package net.strong_links.core

import java.io.File

trait CmdLineTrait {

  trait ValueParser[T] {
    def parse(s: String): T
  }

  def toFile(s: String) = new File(OS.translatePath(s))
  def toInt(s: String) = s.toInt
  def toFloat(s: String) = s.toFloat
  def toDouble(s: String) = s.toDouble

  def handleNull[T](s: String, value: => T) = s match { case null => None; case _ => Some(value) }

  trait StringParser extends ValueParser[String] {
    def parse(s: String) = s
  }

  trait OptionStringParser extends ValueParser[Option[String]] {
    def parse(s: String) = handleNull(s, s)
  }

  trait FileParser extends ValueParser[File] {
    def parse(s: String) = toFile(s)
  }

  trait OptionFileParser extends ValueParser[Option[File]] {
    def parse(s: String) = handleNull(s, toFile(s))
  }

  trait IntParser extends ValueParser[Int] {
    def parse(s: String) = toInt(s)
  }

  trait OptionIntParser extends ValueParser[Option[Int]] {
    def parse(s: String) = handleNull(s, s.toInt)
  }

  trait FloatParser extends ValueParser[Float] {
    def parse(s: String) = toFloat(s)
  }

  trait OptionFloatParser extends ValueParser[Option[Float]] {
    def parse(s: String) = handleNull(s, toFloat(s))
  }

  trait DoubleParser extends ValueParser[Double] {
    def parse(s: String) = toDouble(s)
  }

  trait OptionDoubleParser extends ValueParser[Option[Double]] {
    def parse(s: String) = handleNull(s, toDouble(s))
  }

  trait ItemNature {
    def isParameter: Boolean
    def isBooleanSwitch: Boolean
    def name: String
  }

  class Help(val text: String)

  def help(text: String) = new Help(text)

  abstract class Item[T](pKey: String, val help: String) extends ItemNature {
    val key = pKey.toLowerCase
    def parse(s: String): T
    def nokey = "no" + key
    def displayLength = pKey.length
  }

  abstract class GenericSwitch[T](pKey: String, help: String) extends Item[T](pKey, help) with ItemNature {
    def isParameter = false
    def name = "--" + key
  }

  abstract class VSwitch[T](pKey: String, val valueLabel: String, help: String) extends GenericSwitch[T](pKey, help) {
    def isBooleanSwitch = false
    override def displayLength = pKey.length + valueLabel.length
  }

  class Switch(pKey: String, override val help: String) extends GenericSwitch[Boolean](pKey, help) {
    def isBooleanSwitch = true
    def parse(s: String) = (s == "true")
  }

  def switch(key: String, help: String) = new Switch(key, help)
  def stringSwitch(key: String, valueLabel: String, help: String) = new VSwitch[Option[String]](key, valueLabel, help) with OptionStringParser
  def fileSwitch(key: String, valueLabel: String, help: String) = new VSwitch[Option[File]](key, valueLabel, help) with OptionFileParser
  def intSwitch(key: String, valueLabel: String, help: String) = new VSwitch[Option[Int]](key, valueLabel, help) with OptionIntParser
  def floatSwitch(key: String, valueLabel: String, help: String) = new VSwitch[Option[Float]](key, valueLabel, help) with OptionFloatParser
  def doubleSwitch(key: String, valueLabel: String, help: String) = new VSwitch[Option[Double]](key, valueLabel, help) with OptionDoubleParser

  abstract class VParameter[T](valueLabel: String, help: String) extends Item[T](valueLabel, help) with ItemNature {
    def isParameter = true
    def isBooleanSwitch = false
    def name = "<" + valueLabel + ">"
  }

  def stringParameter(key: String, help: String) = new VParameter[String](key, help) with StringParser
  def fileParameter(key: String, help: String) = new VParameter[File](key, help) with FileParser
  def intParameter(key: String, help: String) = new VParameter[Int](key, help) with IntParser
  def floatParameter(key: String, help: String) = new VParameter[Float](key, help) with FloatParser
  def doubleParameter(key: String, help: String) = new VParameter[Double](key, help) with DoubleParser

  def apply(callingObject: Object, args: Array[String], help: List[Help] = Nil) =
    new CmdLineClass(callingObject, args, help)

  class CmdLineClass(callingObject: Object, args: Array[String], help: List[Help] = Nil) extends Logging {

    private val map = scala.collection.mutable.Map[Item[_], String]()

    private class ItemWrapper[T](i: Item[T]) {
      def v = i.parse(map.getOrElse(i, null))
    }

    private implicit def itemToItemWrapper[T](i: Item[T]) = new ItemWrapper(i)

    private def setMap[T](i: Item[T], s: String) {
      Errors.trap("Element _" << i.name) {
        map(i) = s
        i.v
      }
    }

    def run[A1](p1: Item[A1])(code: (A1) => Unit) = {
      xeq(List(p1)) { code(p1.v) }
    }

    def run[A1, A2](p1: Item[A1], p2: Item[A2])(code: (A1, A2) => Unit) = {
      xeq(List(p1, p2)) { code(p1.v, p2.v) }
    }

    def run[A1, A2, A3](p1: Item[A1], p2: Item[A2], p3: Item[A3])(code: (A1, A2, A3) => Unit) = {
      xeq(List(p1, p2, p3)) { code(p1.v, p2.v, p3.v) }
    }

    def run[A1, A2, A3, A4](p1: Item[A1], p2: Item[A2], p3: Item[A3], p4: Item[A4])(code: (A1, A2, A3, A4) => Unit) = {
      xeq(List(p1, p2, p3, p4)) { code(p1.v, p2.v, p3.v, p4.v) }
    }

    def run[A1, A2, A3, A4, A5](p1: Item[A1], p2: Item[A2], p3: Item[A3], p4: Item[A4], p5: Item[A5])(code: (A1, A2, A3, A4, A5) => Unit) = {
      xeq(List(p1, p2, p3, p4, p5)) { code(p1.v, p2.v, p3.v, p4.v, p5.v) }
    }

    def run[A1, A2, A3, A4, A5, A6](p1: Item[A1], p2: Item[A2], p3: Item[A3], p4: Item[A4], p5: Item[A5], p6: Item[A6])(code: (A1, A2, A3, A4, A5, A6) => Unit) = {
      xeq(List(p1, p2, p3, p4, p5, p6)) { code(p1.v, p2.v, p3.v, p4.v, p5.v, p6.v) }
    }

    def run[A1, A2, A3, A4, A5, A6, A7](p1: Item[A1], p2: Item[A2], p3: Item[A3], p4: Item[A4], p5: Item[A5], p6: Item[A6], p7: Item[A7])(code: (A1, A2, A3, A4, A5, A6, A7) => Unit) = {
      xeq(List(p1, p2, p3, p4, p5, p6, p7)) { code(p1.v, p2.v, p3.v, p4.v, p5.v, p6.v, p7.v) }
    }

    def run[A1, A2, A3, A4, A5, A6, A7, A8](p1: Item[A1], p2: Item[A2], p3: Item[A3], p4: Item[A4], p5: Item[A5], p6: Item[A6], p7: Item[A7], p8: Item[A8])(code: (A1, A2, A3, A4, A5, A6, A7, A8) => Unit) = {
      xeq(List(p1, p2, p3, p4, p5, p6, p7, p8)) { code(p1.v, p2.v, p3.v, p4.v, p5.v, p6.v, p7.v, p8.v) }
    }

    def xeq(items: List[Item[_]])(code: => Unit): Unit = {
      val progName = OS.getFinalClassName(callingObject)
      val logConfigSwitch = switch("log-config", "Log the configuration used.")
      val stackTraceSwitch = switch("stack-trace", "Show stack trace upon error.")
      val predefinedSwitches = List(logConfigSwitch, stackTraceSwitch)
      val allItems = (items ::: predefinedSwitches)
      val par = Util.filterOn[VParameter[_]](allItems)
      val ss = Util.filterOn[Switch](allItems).sortWith(_.key < _.key)
      val vs = Util.filterOn[VSwitch[_]](allItems).sortWith(_.key < _.key)

      var stackTrace = false

      def usage = {
        def w(s: String) = Console.err.println(s)
        def f(fmt: String, i: Item[_], width: Int) {
          val k = width + 6
          val s = i match { case vs: VSwitch[_] => fmt << (vs.key, vs.valueLabel); case _ => fmt << i.key }
          val x = (s + (" " * k)).substring(0, k)
          w("  _ _" << (x, i.help))
        }
        def fl(fmt: String, L: List[Item[_]], width: Int) {
          for (i <- L)
            f(fmt, i, width)
        }
        w("Usage:")
        w("  _ _" << (progName, par.map("<_>" << _.key).mkString(" ")))
        val max = (0 /: allItems)(_ max _.displayLength)
        w("")
        w("Parameters:")
        fl("<_>", par, max)
        if (!ss.isEmpty || !vs.isEmpty) {
          w("")
          w("Optional switches:")
          fl("--_", ss, max)
          fl("--_ <_>", vs, max)
          w("")
          w("Switches can appear anywhere in the command line (before, ")
          w("after or even between the parameters), in any order.")
          w("")
          w("Switch names are case-insensitive and they can be prefixed")
          w("by 'no' (ignored in this case).")
        }
        if (help != Nil) {
          w("")
          for (h <- help)
            w(h.text)
        }
        w("")
        OS.exitSuccess
      }

      def processSwitch(key: String)(getNextArg: => Option[String]) {
        def next = getNextArg match {
          case None => Errors.fatal(("No value supplied for switch --_" << key): String)
          case Some(v) => v
        }
        def setIf(list: List[Item[_]], keyUsed: Item[_] => String, value: => String) = {
          list.filter(keyUsed(_) == key) match {
            case Nil => false
            case List(i) => setMap(i, value); true
            case _ => Errors.fatal("Key _ mismatch." << key)
          }
        }
        if (!setIf(ss, _.key, "true") && !setIf(ss, _.nokey, "false") &&
          !setIf(vs, _.key, next) && !setIf(vs, _.nokey, { next; null }))
          Errors.fatal("_ is not a valid switch." << key)
      }

      def logConfig {
        def show(title: String, list: List[Item[_]]) {
          if (!list.isEmpty) {
            logInfo("Run-time " + title)
            list.foreach(p => { val x = ("  _ = " << p.name) + ("_" <<< p.v); logInfo(x) })
          }
        }
        show("parameters", par)
        show("switches", ss)
        show("valued switches", vs)
      }

      def parse {
        var ab = scala.collection.mutable.ArrayBuffer[String]()
        var i = 0
        while (i < args.length) {
          if (args(i).startsWith("--"))
            processSwitch(args(i).substring(2).toLowerCase) {
              i += 1
              if (i == args.length)
                None
              else
                Some(args(i))
            }
          else
            ab += args(i)
          i += 1
        }
        if (ab.length != par.length)
          Errors.fatal("Invalid number of arguments; _ were provided, _ were expected." << (ab.length, par.length))
        for (i <- 0 until ab.length)
          setMap(par(i), ab(i))
      }

      def run = Errors.trap("Program !_" << progName) {
        if (args.length == 1 && args(0) == "--help")
          usage
        logInfo("Program _ started." << progName)
        Errors.trap("Command line error", "Use --help for help.") {
          parse
          stackTrace = stackTraceSwitch.v
        }
        Errors.trap("Run-time error") {
          if (logConfigSwitch.v)
            logConfig
          code
        }
        logInfo("Program _ ended successfully." << progName)
      }

      try run catch {
        case e =>
          logError(e, stackTrace)
          OS.exitError
      }
    }
  }
}

object CmdLine extends CmdLineTrait
