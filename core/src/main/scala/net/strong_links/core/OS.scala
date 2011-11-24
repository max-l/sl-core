package net.strong_links.core

import scala.sys.process._

class ExecuteCommandResults(val cmd: String, val exitValue: Int, val output: String)

object OS {

  def executeCommand(cmd: String, params: Array[String]): ExecuteCommandResults = {
    def grabOutput(sb: StringBuffer, s: String) {
      def ignored(s: String) = {
        s == ""
      }
      val line = s.trim
      if (!ignored(line)) {
        sb.append(line)
        sb.append("\n")
      }
    }
    val p = Process(cmd, params)
    var sb = new StringBuffer
    val handler = (s: String) => grabOutput(sb, s)
    val exitValue = p ! ProcessLogger(handler, handler)
    new ExecuteCommandResults((cmd /: params)(_ + " \"" + _ + "\""), exitValue, sb.toString.trim)
  }
}
