package org.clulab.wm.eidos.apps.filter

import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.json4s.DefaultFormats
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

import java.io.File

object FilterJsonText extends App {

  class Filter(outputDir: String) {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    def filter(jValue: JValue, inputFile: File): Unit = {
      val jString: JValue = jValue \ "extracted_text"
      val text: String = jString.extract[String]
      val path = FileEditor(inputFile).setDir(outputDir).setExt("txt").get

      FileUtils.printWriterFromFile(path).autoClose { pw =>
        pw.println(text)
      }
    }
  }

  val inputDir = args(0)
  val outputDir = args(1)
  val filter = new Filter(outputDir)
  val inputFiles = FileUtils.findFiles(inputDir, "json")

  inputFiles.sortBy(_.getName).foreach { inputFile =>
    val text = FileUtils.getTextFromFile(inputFile)
    val json = JsonMethods.parse(text)

    filter.filter(json, inputFile)
  }
}
