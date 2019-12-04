package org.clulab.wm.eidos.apps.batch

import java.io.File

import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.utils.StringUtils
import org.json4s.DefaultFormats
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

object FilterCdrExtractedText extends App {

  class Filter(outputDir: String) {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    def filter(inputFile: File, jValue: JValue): Unit = {
      println(s"Extracting from ${inputFile.getName}")
      val jString: JValue = jValue \ "extracted_text"
      val text: String = jString.extract[String]
      val outputFile = new File(outputDir + "/" + StringUtils.beforeLast(inputFile.getName, '.')) + ".txt"

      FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
        printWriter.print(text)
      }
    }
  }

  val inputDir = args(0)
  val outputDir = args(1)
  val inputFiles = findFiles(inputDir, "json")
  val filter = new Filter(outputDir)

  inputFiles.foreach { inputFile =>
    val text = FileUtils.getTextFromFile(inputFile)
    val json = JsonMethods.parse(text)
    filter.filter(inputFile, json)
  }
}
