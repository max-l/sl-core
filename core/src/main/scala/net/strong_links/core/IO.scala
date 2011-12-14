package net.strong_links.core

import java.io._
import scala.io._

object IO {

  val dirSeparator = System.getProperty("file.separator")

  val dirSeparatorChar = {
    if (dirSeparator.length != 1)
      Errors.fatal("Invalid file.separator property; not length 1 but _." << dirSeparator.length)
    dirSeparator(0)
  }

  def checkForExistingDirectory(file: File) {
    if (!file.exists)
      Errors.fatal("Directory _ does not exist." << file.getCanonicalPath)
    if (!file.isDirectory)
      Errors.fatal("File _ is not a directory." << file.getCanonicalPath)
  }

  def copy(inFile: File, outFile: File, mightOverwrite: Boolean = true) {
    val bufsiz = 1024 * 1000
    val b = new Array[Byte](bufsiz)
    val exists = outFile.exists
    if (exists && !mightOverwrite)
      Errors.fatal("Destination file _ already exists and cannot be overwritten." << outFile.getCanonicalPath)
    createDirectory(outFile.getParentFile)
    val is = new FileInputStream(inFile)
    val os = new FileOutputStream(outFile)
    var done = false
    var offset = 0
    while (!done) {
      try {
        val n = is.read(b)
        try {
          if (n > 0)
            os.write(b, 0, n)
        } catch { case e: Exception => Errors.fatal("Writing _ bytes in _ at offset _" << (n, outFile.getCanonicalPath, offset), e) }
        done = n < bufsiz
        offset += n
      } catch { case e: Exception => Errors.fatal("Reading _ bytes in _ at offset _" << (bufsiz, inFile.getCanonicalPath, offset), e) }
    }
    is.close
    os.close
  }

  def scanDirectory(directory: File, filter: File => Boolean)(work: File => Unit): Unit = {
    if (!directory.exists)
      Errors.fatal("Directory _ does not exist." << directory)
    if (!directory.isDirectory)
      Errors.fatal("File _ is not a directory." << directory)
    val files = directory.listFiles
    if (files == null)
      Errors.fatal("Can't list files in directory _." << directory.getCanonicalPath)
    for (f <- files) {
      if (f.isDirectory)
        scanDirectory(f, filter) { work }
      else if (filter(f))
        work(f)
    }
  }

  def scanDirectory(directory: File)(work: File => Unit): Unit = {
    scanDirectory(directory, (f) => true) { work }
  }

  def processDirectories(directory: File)(work: File => Unit): Unit = {
    if (!directory.exists)
      Errors.fatal("Directory _ does not exist." << directory)
    if (!directory.isDirectory)
      Errors.fatal("File _ is not a directory." << directory)
    for (f <- directory.listFiles)
      if (f.isDirectory) {
        work(f)
        processDirectories(f) { work }
      }
  }

  def writeUtf8ToFile(file: File, contents: String) {
    val ps = new PrintStream(file, "UTF-8")
    ps.append(contents)
    ps.close
  }

  def writeUtf8ToFile(fileName: String, contents: String) {
    writeUtf8ToFile(new File(fileName), contents)
  }

  def createTemporaryFile: File = {
    File.createTempFile("tmp", ".tmp")
  }

  def loadUtf8TextFile(f: File): String = {
    val source = Source.fromFile(f, "utf-8")
    val results = source.mkString
    source.close
    results
  }

  def loadBinaryFile(f: File): Array[Byte] = {
    if (f.length > Int.MaxValue)
      Errors.fatal("File _ is too long." << f.getCanonicalPath)
    val toRead = f.length.toInt
    val buffer = new Array[Byte](toRead)
    val fis = new FileInputStream(f)
    val bytesRead = fis.read(buffer)
    if (bytesRead != toRead)
      Errors.fatal("Only _ bytes read in file _, expected _." << (bytesRead, f.getCanonicalPath, toRead))
    fis.close
    buffer
  }

  def fileUuid(file: File) = {
    val md5 = Util.md5(IO.loadBinaryFile(file))
    if (md5.length != 16)
      Errors.fatal("MD5 has a length _ instead of 16." << md5.length)
    Util.encodeLongFromBytes(md5, 0) + Util.encodeLongFromBytes(md5, 8)
  }

  def usingCharStream(f: (CharStream) => Unit): String = {
    val cs = new CharStream
    f(cs)
    cs.close
  }

  def LeveledCharStream = new LeveledCharStream

  def usingLeveledCharStream(f: (LeveledCharStream) => Unit): String = {
    val cs = LeveledCharStream
    f(cs)
    cs.close
  }

  private def createSingleDirectory(directory: File) {
    if (directory.exists && !directory.isDirectory)
      Errors.fatal("Existing path _ is not a directory as expected." << directory.getCanonicalPath)
    if (!directory.exists)
      if (!directory.mkdir)
        Errors.fatal("Can't create directory _." << directory.getCanonicalPath)
  }

  def createDirectory(directory: File, mightAlreadyExist: Boolean = true) {
    val exists = directory.exists
    if (exists && !mightAlreadyExist)
      Errors.fatal("Directory _ already exists." << directory.getCanonicalPath)
    if (!exists) {
      val segments = Util.split(directory.getCanonicalPath, IO.dirSeparator)
      for (n <- 1 to segments.length)
        createSingleDirectory(new File(segments.take(n).mkString(IO.dirSeparator)))
    }
  }

  lazy val currentDirSuffix = IO.dirSeparator + "."

  def toCanonicalPath(fileName: String) = {
    new File(fileName).getCanonicalPath
  }

  def currentDirectory = {
    new File(".")
  }

  def deleteFile(f: File, mightNotExist: Boolean = true) {
    val exists = f.exists
    if (!exists && !mightNotExist)
      Errors.fatal("File _ does not exist." << f.getCanonicalPath)
    if (exists)
      if (!f.delete)
        Errors.fatal("Can't delete file _." << f.getCanonicalPath)
  }

  def renameFile(from: File, to: File, mightOverwrite: Boolean = true) {
    val exists = to.exists
    if (exists && !mightOverwrite)
      Errors.fatal("Destination file _ already exists and cannot be overwritten." << to.getCanonicalPath)
    deleteFile(to, !exists)
    if (!from.renameTo(to))
      Errors.fatal("Can't rename file _ to _." << (from.getCanonicalPath, to.getCanonicalPath))
  }

  def checkDirectory(directory: File, createIfDoesNotExist: Boolean = false) {
    if (!directory.exists)
      if (createIfDoesNotExist)
        createDirectory(directory, false)
      else
        Errors.fatal("Directory _ does not exist." << directory.getCanonicalPath)
    if (!directory.isDirectory)
      Errors.fatal("File _ is not a directory." << directory.getCanonicalPath)
  }
}

class CharStream {
  private var active = true
  val sb = new StringBuilder

  def print(s: String) { sb.append(s) }
  def println(s: String) { print(s); println }
  def println { print("\n") }
  def printIf(b: Boolean, s: String) = if (b) print(s)
  def printlnIf(b: Boolean, s: String) = if (b) println(s)

  def close = {
    active = false
    sb.toString
  }
}

class LeveledCharStream extends CharStream {

  private var level = 0

  def increaseLevel { level += 1 }

  def decreaseLevel { level -= 1; if (level < 0) Errors.fatal("Level dropped below 0.") }

  override def println = super.println

  override def println(s: String) = {
    def normalize(s: String) = if (s.trim.isEmpty) "" else s
    val margin = " " * (level * 2)
    Util.split(s).foreach(line => super.println(normalize(margin + line)))
  }

  def block(s: String, start: String = "{", end: String = "}")(code: => Unit) {
    println
    println(s + " " + start)
    increaseLevel
    code
    decreaseLevel
    println(end)
  }
}

