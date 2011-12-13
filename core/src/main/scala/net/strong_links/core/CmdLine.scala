package net.strong_links.core

import java.io.File

trait CmdLineTrait {
  trait ValueParser[T] {
    def parse(s: String): T
  }

  def toFile(s: String) = new File(OS.translatePath(s))
  def toInt(s: String) = s.toInt
  def toFloat(s: String) = s.toFloat

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
  }

  abstract class GenericSwitch[T](pKey: String, help: String) extends Item[T](pKey, help) with ItemNature {
    def isParameter = false
    def name = "--" + key
  }

  abstract class VSwitch[T](pKey: String, val valueLabel: String, help: String) extends GenericSwitch[T](pKey, help) {
    def isBooleanSwitch = false
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

  abstract class VParameter[T](valueLabel: String, help: String) extends Item[T](valueLabel, help) with ItemNature {
    def isParameter = true
    def isBooleanSwitch = false
    def name = "<" + valueLabel + ">"
  }

  def stringParameter(key: String, help: String) = new VParameter[String](key, help) with StringParser
  def fileParameter(key: String, help: String) = new VParameter[File](key, help) with FileParser
  def intParameter(key: String, help: String) = new VParameter[Int](key, help) with IntParser
  def floatParameter(key: String, help: String) = new VParameter[Float](key, help) with FloatParser

  def apply(callingObject: Object, args: Array[String], help: List[Help] = Nil) =
    new CmdLineClass(callingObject, args, help)

  class CmdLineClass(callingObject: Object, args: Array[String], help: List[Help] = Nil) {

    private val map = scala.collection.mutable.Map[Item[_], String]()

    private class ItemWrapper[T](i: Item[T]) {
      def v = i.parse(map.getOrElse(i, null))
    }

    private implicit def itemToItemWrapper[T](i: Item[T]) = new ItemWrapper(i)

    private def setMap[T](i: Item[T], s: String) {
      Errors.context("Element _" << i.name) {
        map(i) = s
        i.v
      }
    }

    def run[A1](p1: Item[A1])(code: (Xlogger, A1) => Unit) = {
      xeq(List(p1)) { logger => code(logger, p1.v) }
    }

    def run[A1, A2](p1: Item[A1], p2: Item[A2])(code: (Xlogger, A1, A2) => Unit) = {
      xeq(List(p1, p2)) { logger => code(logger, p1.v, p2.v) }
    }

    def run[A1, A2, A3](p1: Item[A1], p2: Item[A2], p3: Item[A3])(code: (Xlogger, A1, A2, A3) => Unit) = {
      xeq(List(p1, p2, p3)) { logger => code(logger, p1.v, p2.v, p3.v) }
    }

    def run[A1, A2, A3, A4](p1: Item[A1], p2: Item[A2], p3: Item[A3], p4: Item[A4])(code: (Xlogger, A1, A2, A3, A4) => Unit) = {
      xeq(List(p1, p2, p3, p4)) { logger => code(logger, p1.v, p2.v, p3.v, p4.v) }
    }

    def run[A1, A2, A3, A4, A5](p1: Item[A1], p2: Item[A2], p3: Item[A3], p4: Item[A4], p5: Item[A5])(code: (Xlogger, A1, A2, A3, A4, A5) => Unit) = {
      xeq(List(p1, p2, p3, p4, p5)) { logger => code(logger, p1.v, p2.v, p3.v, p4.v, p5.v) }
    }

    def run[A1, A2, A3, A4, A5, A6](p1: Item[A1], p2: Item[A2], p3: Item[A3], p4: Item[A4], p5: Item[A5], p6: Item[A6])(code: (Xlogger, A1, A2, A3, A4, A5, A6) => Unit) = {
      xeq(List(p1, p2, p3, p4, p5, p6)) { logger => code(logger, p1.v, p2.v, p3.v, p4.v, p5.v, p6.v) }
    }

    def xeq[R](items: List[Item[_]])(code: Xlogger => R): R = {

      val progName = OS.getFinalClassName(callingObject)
      val logger = new Xlogger(new StandAloneLogger)
      var stackTrace = false

      val logConfigSwitch = switch("logConfig", "Log the configuration used.")
      val debugSwitch = switch("debug", "Activate debug mode.")
      val stackTraceSwitch = switch("stackTrace", "Show stack trace upon error.")
      val predefinedSwitches: List[Item[_]] = List(logConfigSwitch, debugSwitch, stackTraceSwitch)
      val allItems = items ::: predefinedSwitches
      val (par, switches) = (allItems).partition(_.isParameter)
      val (ss, vs) = switches.sortWith(_.key < _.key).partition(_.isBooleanSwitch)

      def usage {
        def w(s: String) = Console.err.println(s)
        def maxLength(i: Item[_]) = i match { case vs: VSwitch[_] => vs.key.length + vs.valueLabel.length; case _ => i.key.length }
        def f(fmt: String, i: Item[_], width: Int) {
          val k = width + 4
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
        val max = (0 /: (ss ::: vs ::: par))((a, b) => scala.math.max(a, maxLength(b)))
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

      def logConfig(logger: Xlogger) {
        def show(title: String, list: List[Item[_]]) {
          if (!list.isEmpty) {
            logger.info("Run-time " + title)
            list.foreach(p => { val x = ("  _ = " << p.name) + ("_" <<< p.v); logger.info(x) })
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

      try {
        if (args.length == 1 && args(0) == "--help") {
          usage
          OS.exitSuccess
        }
        logger.info("Program _ started." << progName)
        Errors.context("Program _ command line error" << progName, "Use --help for help.") {
          parse
        }
        val result = Errors.context("Program _ run-time error" << progName) {
          logger.enableDebug = debugSwitch.v
          if (logConfigSwitch.v)
            logConfig(logger)
          code(logger)
        }
        logger.info("Program _ ended." << progName)
        result
      } catch {
        case e: Exception =>
          if (stackTrace)
            throw e
          else {
            logger.error(e)
            OS.exitError
          }
      }
    }
  }
}

object CmdLine extends CmdLineTrait
