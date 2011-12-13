package net.strong_links.core

import java.io.File

object CodeGeneration {

  // Note: file names written without backslashes because it makes Eclipse complain
  //       about invalid unicode sequences...
  def header(cs: LeveledCharStream, sourceName: String, destinationFileName: String) {
    def r(s: String) = s.replace("\\", "/")
    val of = r(destinationFileName)
    val sf = r(sourceName)
    // Box has 80 characters wide, or more if needed.
    val pad = 15
    val stars = (80 /: Seq(of, sf))(_ max _.length + pad) - 1
    cs.println("/" + "*" * stars)
    cs.println("*")
    cs.println("* Output file: _" << of)
    cs.println("*")
    cs.println("* Source:      _" << sf)
    cs.println("*")
    cs.println("*" * stars + "/")
    cs.println
  }

  def generateScalaFile[T](entries: Seq[T], outputFile: File, sourceFile: File,
    masterPackageName: String, packageName: String, className: String, objectName: String, objectIsInside: Boolean, imports: List[String])(code: T => String) {
    val cs = new LeveledCharStream
    def genObjectIf(b: Boolean) = if (b) {
      cs.println
      cs.println("package object _ extends _._._" << (objectName, masterPackageName, packageName, className))
    }
    header(cs, sourceFile.getCanonicalPath, outputFile.getCanonicalPath)
    cs.block("package _" << masterPackageName) {
      if (!imports.isEmpty) {
        cs.println
        for (imp <- imports)
          cs.println("import _" << imp)
      }
      cs.block("package _" << packageName) {
        cs.block("class _" << className) {
          entries.foreach(e => cs.println(code(e)))
        }
        genObjectIf(objectIsInside)
      }
      genObjectIf(!objectIsInside)
    }
    IO.createDirectory(outputFile.getParentFile, true)
    IO.writeUtf8ToFile(outputFile, cs.close)
  }
}