package net.strong_links.core

class PluggedString(ps: PluggableString, args: Option[Seq[Any]], quotedDefault: Boolean)
  extends PluggableString(ps.s) {

  override def toString = format(failsafe = false, quoted = quotedDefault)

  def format(failsafe: Boolean, quoted: Boolean): String = {
    PluggedArguments.format(ps.s, args, failsafe, quoted)
  }
}
