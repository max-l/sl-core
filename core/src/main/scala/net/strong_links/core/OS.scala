package net.strong_links.core

import scala.sys.process._

class ExecuteCommandResults(val cmd: String, val exitValue: Int, val output: String)

object OS {

  def exitCodeSuccess = 0
  def exitCodeError = 1

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

  def exit(conditionCode: Int): Nothing = {
    sys.exit(conditionCode)
  }

  def exitSuccess: Nothing = exit(exitCodeSuccess)

  def exitError: Nothing = exit(exitCodeError)

  def getFinalClassName(obj: Object) = {
    val name = obj.getClass.getName
    val shortName = Util.split(name, ".").last
    if (shortName.endsWith("$"))
      shortName.substring(0, shortName.length - 1)
    else
      shortName
  }

  private def toWindows(path: String): String = {
    val s = Util.split(path, '/')
    if (s.length < 2)
      Errors.fatal("Expected at least two segments in _, found _. " << (path, s.length))
    if (s.head == "") {
      // Starts by '/', so it is an absolute path. 
      // Skip the first empty list element.
      val x = s.tail
      "_:\\_" << (x.head, x.tail.mkString("\\"))
    } else
      // Else it is a relative path.
      s.mkString("\\")
  }

  // Allow the manipulation of Unix paths in a Linux shell console.
  def translatePath(path: String) = {
    if (path.contains('/') && (IO.dirSeparatorChar == '\\'))
      toWindows(path)
    else
      path
  }
}

