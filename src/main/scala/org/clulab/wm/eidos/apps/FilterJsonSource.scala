package org.clulab.wm.eidos.apps

import java.io.File

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.{FileEditor, FileUtils, StringUtils}
import org.json4s.DefaultFormats
import org.json4s.JObject
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

object FilterJsonSource extends App {

  class Filter(outputDir: String) {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    def filter(jValue: JValue, inputFile: File): Unit = {
      val extractions: JValue = jValue \ "_source"

      extractions match {
        case jObject: JObject =>
          val json = stringify(jObject, pretty = true)
          val path = FileEditor(inputFile).setDir(outputDir).get

          FileUtils.printWriterFromFile(path).autoClose { pw =>
            pw.println(json)
          }

        case _ => throw new RuntimeException(s"Unexpected extractions value: $extractions")
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
