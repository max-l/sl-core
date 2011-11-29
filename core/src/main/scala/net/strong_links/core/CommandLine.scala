package net.strong_links.core

import net.strong_links.core._

object CommandLine {

  class Item(val name: String, val id: String, val help: String, action: (String) => Unit) {
    def run(s: String) = try action(s) catch {
      case e: Exception => Errors.fatal("Invalid command line item _: _" << (name, e.getMessage))
    }
  }

  class Help(helpText: String) extends Item(helpText, "", "", s => {})

  object Help{
    def apply(helpText: String) = new Help(helpText)
  }

  class SimpleSwitch(name: String, help: String, action: => Unit) extends Item(name, "", help, s => action)

  object SimpleSwitch {
    def apply(name: String, help: String, action: => Unit) = new SimpleSwitch(name, help, action)
  }

  class ValuedSwitch(name: String, id: String, help: String, action: (String)=> Unit) extends Item(name, id, help, action)

  object ValuedSwitch {
    def apply(name: String, id: String, help: String, action: (String)=> Unit) = new ValuedSwitch(name, id, help, action)
  }

  class Parameter(name: String, help: String, action: (String)=> Unit) extends Item(name, "", help, action)

  object Parameter {
    def apply(name: String, help: String, action: (String)=> Unit) = new Parameter(name, help, action)
  }

  def apply(programName: String, rules: List[Item], args: Array[String]) {
    new CommandLine(programName, rules).run(args)
  }
}

import CommandLine._

class CommandLine(progName: String, rules: List[Item]) {
  
  val ss = Util.filterOn[SimpleSwitch](rules)
  val vs = Util.filterOn[ValuedSwitch](rules)
  val par = Util.filterOn[Parameter](rules)
  val help = Util.filterOn[Help](rules)
  
  def usage {
    def w(s: String) = Console.err.println(s)
    def f(fmt: String, i: Item, width: Int) {
      val k = width + 4
      val s = if (i.id != "") fmt << (i.name, i.id) else fmt << i.name
      val x = (s + (" " * k)).substring(0, k)
      w("  _ _" << (x, Util.endDot(i.help)))
    }
    def fl(fmt: String, L: List[Item], width: Int) {
      for (i <- L)
        f(fmt, i: Item, width)
    }
    w("Usage:")
    w("  _ _" << (progName, par.map("<_>" << _.name).mkString(" ")))
    val max = (0 /: (ss ::: vs ::: par))((a, b) => scala.math.max(a, b.name.length + b.id.length))
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
    }
    if (help != Nil) {
      w("")
      for (h <- help)
        w(h.name)
    }
    w("")
  }

  def mightRun(args: Array[String]) {
    var argsLeftBuffer = scala.collection.mutable.ArrayBuffer[String]()
    var i = 0
    while (i < args.length) {
      if (args(i).startsWith("--")) {
        // Switch
        def noValue = Errors.fatal("No value supplied to the switch _." << args(i))
        val name = args(i).substring(2)
        ss.find(_.name == name) match {
          case Some(s) => 
            s.run("") 
          case None =>          
            vs.find(_.name == name) match {
              case Some(s) =>
                if (i == args.length - 1)
                  noValue
                i += 1
                if (args(i).startsWith("--"))
                  noValue
                s.run(args(i)) 
              case None =>
                Errors.fatal("_ is not a valid switch." << args(i))
            }
        }
      } else {
        // Not a switch
        argsLeftBuffer += args(i)
      }
      i += 1
    }
    val argsLeft = argsLeftBuffer.toArray
    if (argsLeft.length != par.length)
      Errors.fatal("Invalid number of arguments; _ were provided, _ were expected." << (argsLeft.length, par.length))
    for (i <- 0 until argsLeft.length)
      par(i).run(argsLeft(i)) 
  }

  def run(args: Array[String]) {
    if (args.length == 1 && args(0) == "--help") {
      usage
      System.exit(0)
    }
    try mightRun(args) catch {
      case e: Exception =>
        val x = Util.endDot(e.getMessage)
        val msg = ("Command line error: _ Use --help for help." << x).format(true, false)
        Errors.fatal(msg)  
    }
  }
}
