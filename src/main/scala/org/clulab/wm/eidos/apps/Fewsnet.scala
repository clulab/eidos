package org.clulab.wm.eidos.apps

import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils

import scala.collection.JavaConverters._

import java.io.File

/**
  * This recursively searches the dir for all pdf files and runs pdf_to_txt.py
  * (in the src/main/python directory) as well as pdfinfo, which should be in
  * the operating system's path.
  */
object Fewsnet extends App {
  val file = new File(args(0))
  require(file.isDirectory)

  def pdfToInfo(inputFile: File): Unit = {
    val outputFile = new FileEditor(inputFile).setExt(".info").get
    if (!outputFile.exists) {
      val arguments = List(
        "pdfinfo",
        inputFile.getCanonicalPath,
      ).map(_.replace('/', File.separatorChar))
      val processBuilder = new ProcessBuilder(arguments.asJava)

      processBuilder.redirectInput(ProcessBuilder.Redirect.INHERIT)
      processBuilder.redirectOutput(outputFile)
      processBuilder.redirectError(ProcessBuilder.Redirect.INHERIT)

      val process = processBuilder.start()
      process.waitFor()
    }
  }

  def pdfToText(inputFile: File): Unit = {
    val outputFile = new FileEditor(inputFile).setExt(".txt").get
    if (!outputFile.exists) {
      val arguments = List(
        "python",
        "./src/main/python/pdf_to_txt_file.py",
        inputFile.getCanonicalPath,
        outputFile.getCanonicalPath
      ).map(_.replace('/', File.separatorChar))
      val processBuilder = new ProcessBuilder(arguments.asJava)

      processBuilder.redirectInput(ProcessBuilder.Redirect.INHERIT)
      processBuilder.redirectOutput(ProcessBuilder.Redirect.INHERIT)
      processBuilder.redirectError(ProcessBuilder.Redirect.INHERIT)

      val process = processBuilder.start()
      process.waitFor()
    }
  }

  FileUtils
      .walkTree(file)
      .filter(_.getName.endsWith(".pdf"))
      .foreach { file =>
        println(file.getCanonicalPath)
        pdfToInfo(file)
        pdfToText(file)
      }
}
