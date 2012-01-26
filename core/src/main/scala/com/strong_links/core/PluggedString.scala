package com.strong_links.core

class PluggedString(ps: PluggableString, args: Option[Seq[Any]], quotedDefault: Boolean)
  extends PluggableString(ps.s) {

  //println("Create " + ps.s)

  override def toString = {
    //println("Invoke")
    format(failsafe = false, quoted = quotedDefault)
  }

  def format(failsafe: Boolean, quoted: Boolean): String = {
    PluggedArguments.format(ps.s, args, failsafe, quoted)
  }
}
