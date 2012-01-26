package com.strong_links.core.codegen

import com.strong_links.core._
import java.io.File

trait CodeGeneration extends Logging {

  def computePackageNameSegments(rootDirectory: File, file: File, rootPackage: Option[String]) = {
    val context = "Package name computation failed for _, input _ and root package _."
    Errors.trap(context << (file, rootDirectory, rootPackage)) {
      val directory = if (file.isDirectory) file else file.getParentFile
      val partialPath = IO.getRelativePath(rootDirectory, directory)
      val segments = Util.split(partialPath, IO.dirSeparator).filter(!_.isEmpty)
      val rootPackageSegments = rootPackage match {
        case Some(rp) => Util.split(rp, '.')
        case None => Nil
      }
      val results = rootPackageSegments ::: segments
      I18nConfig.checkPackageSegments(results)
      results
    }
  }

  // Note: file names written without backslashes because it makes Eclipse complain
  //       about invalid unicode sequences...
  def header(cs: LeveledCharStream, source: File, destination: File) {
    def r(s: String) = s.replace("\\", "/")
    val of = r(destination.path)
    val sf = r(source.path)
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
    def genObjectIf(b: Boolean, what: String, element: String) = if (b) {
      cs.println
      cs.println("_ _ _ _._._" << (what, objectName, element, masterPackageName, packageName, className))
    }
    header(cs, sourceFile, outputFile)
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
        genObjectIf(objectIsInside, "object", "extends")
      }
      genObjectIf(!objectIsInside, "package object", "extends")
    }
    IO.createDirectory(outputFile.getParentFile, true)
    IO.writeUtf8ToFile(outputFile, cs.close)
  }
}