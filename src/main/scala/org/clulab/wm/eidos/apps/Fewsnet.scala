package org.clulab.wm.eidos.apps

import org.clulab.wm.eidos.EidosOptions
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.jsonld.JLDCorpus
import org.clulab.wm.eidos.utils.meta.PdfInfoText
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.Logging
import org.clulab.wm.eidoscommon.utils.Sourcer

import scala.collection.JavaConverters._
import java.io.File
import scala.io.Source

/**
  * This recursively searches the dir for all pdf files and runs pdf_to_txt_file.py
  * (in the src/main/python directory) as well as pdfinfo, which should be in
  * the operating system's path.
  */
object Fewsnet extends App with Logging {
  val file = new File(args(0))
  require(file.isDirectory)

  def pdfToInfo(inputFile: File): Unit = {
    val outputFile = FileEditor(inputFile).setExt(".info").get
    if (!outputFile.exists) {
      val arguments = List(
        "pdfinfo",
        "-isodates",
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
    val outputFile = FileEditor(inputFile).setExt(".txt").get
    if (!outputFile.exists) {
      val arguments = List(
        "python3",
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

  def read(file: File, reader: EidosSystem, options: EidosOptions): Unit = {
    val textFile = FileEditor(file).setExt(".txt").get
    val infoFile = FileEditor(file).setExt(".info").get
    val jsonldFile = FileEditor(file).setExt(".jsonld").get

    if (textFile.exists && !jsonldFile.exists) {
      val pdfInfo = PdfInfoText(textFile, infoFile)
      val text = pdfInfo.getText
      val metadata = pdfInfo.getMetadata

      try {
        val annotatedDocument = reader.extractFromText(text, options, metadata)

        FileUtils.printWriterFromFile(jsonldFile).autoClose { printWriter =>
          new JLDCorpus(annotatedDocument).serialize(printWriter)
        }
      }
      catch {
        case exception: Exception =>
          logger.error(s"Exception for file $file", exception)
      }
    }
  }

  def convertAll(): Unit = {
    FileUtils
        .walkTree(file)
        .filter(_.getName.endsWith(".pdf"))
        .foreach { file =>
          println(file.getCanonicalPath)
          pdfToInfo(file)
          pdfToText(file)
        }
  }

  def readAll(): Unit = {
    val eidos = new EidosSystem()
    val options = EidosOptions()

    FileUtils
        .walkTree(file)
        .filter(_.getName.endsWith(".pdf"))
        .foreach { file =>
          read(file, eidos, options)
        }
  }

  //convertAll()
  readAll()
}
