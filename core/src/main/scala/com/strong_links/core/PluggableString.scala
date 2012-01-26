package com.strong_links.core

class PluggableString(val s: String) { 

  def << (args: Any*) = {
    new PluggedString(this, Some(args), false)
  }

  def <<< (args: Any*) = {
    new PluggedString(this, Some(args), true)
  }
}
