package org.clulab.wm.eidos.apps

import java.io.File

import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.json4s.DefaultFormats
import org.json4s.JArray
import org.json4s.JNothing
import org.json4s.JString
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

object FilterJson extends App {

  class Filter(val outputDir: String) {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    def filter(inputFile: File, jValue: JValue): Unit = {
      // println(s"Extracting from ${inputFile.getName}")
      val geoLocations: JValue = (jValue \\ "geolocs" \ "text")

      geoLocations match {
        case JArray(geoLocations: List[JString]) =>
          geoLocations.foreach { geoLocation: JString =>
            println(geoLocation.extract[String])
          }
        case JNothing =>
        case _ => throw new RuntimeException(s"Unexpected geoLocations value: $geoLocations")
      }
    }
  }

  val inputDir = args(0)
  val extension = args(1)
  val outputDir = args(2)
  val inputFiles = findFiles(inputDir, extension)
  val filter = new Filter(outputDir)

  inputFiles.foreach { inputFile =>
    val text = FileUtils.getTextFromFile(inputFile)
    val json = JsonMethods.parse(text)
    filter.filter(inputFile, json)
  }
}
