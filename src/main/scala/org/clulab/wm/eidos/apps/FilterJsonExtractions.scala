package org.clulab.wm.eidos.apps

import java.io.File

import org.clulab.wm.eidos.utils.FileUtils
import org.json4s.DefaultFormats
import org.json4s.JArray
import org.json4s.JNothing
import org.json4s.JObject
import org.json4s.JString
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

object FilterJsonExtractions extends App {

  class Filter() {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    def filter(inputFile: File, jValue: JValue): Unit = {
      println(s"Extracting from ${inputFile.getName}")
      val extractions: JValue = (jValue \\ "extractions")

      extractions match {
        case JArray(extractions: List[_]) => // Type erasure removes the [JObject]
          extractions.foreach { extraction =>
            val jString = (extraction \ "text")
            val text = jString.extract[String]
            val oneLiner = text
                .replace("\n", "\\n")
                .replace("\t", "\\t")

            println("\t" + oneLiner)
          }
        case JObject(_) =>
        case _ => throw new RuntimeException(s"Unexpected extractions value: $extractions")
      }
    }
  }

  val inputDir = args(0)
  val extension = args(1)
  val inputFiles = FileUtils.findFiles(inputDir, extension)
  val filter = new Filter()

  inputFiles.foreach { inputFile =>
    val text = FileUtils.getTextFromFile(inputFile)
    val json = JsonMethods.parse(text)
    filter.filter(inputFile, json)
  }
}
