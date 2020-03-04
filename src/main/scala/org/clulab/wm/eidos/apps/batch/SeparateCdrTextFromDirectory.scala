package org.clulab.wm.eidos.apps.batch

import java.io.File

import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileEditor
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.StringUtils
import org.clulab.wm.eidos.utils.meta.CluText
import org.json4s.DefaultFormats
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

object SeparateCdrTextFromDirectory extends App {

  class Filter(outputDir: String) {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    def filter(inputFile: File, jValue: JValue): Unit = {
      println(s"Extracting from ${inputFile.getName}")
      val jString: JValue = jValue \ "extracted_text"
      val text: String = jString.extract[String]
      val outputFile = FileEditor(inputFile).setDir(outputDir).setExt("txt").get

      FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
        printWriter.print(text)
      }
    }
  }

  val inputDir = args(0)
  val outputDir = args(1)
  val inputFiles = FileUtils.findFiles(inputDir, "json")
  val filter = new Filter(outputDir)

  inputFiles.foreach { inputFile =>
    val json = CluText.getJValue(inputFile)

    filter.filter(inputFile, json)
  }
}
