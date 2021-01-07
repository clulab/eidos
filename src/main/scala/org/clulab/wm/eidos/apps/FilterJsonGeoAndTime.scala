package org.clulab.wm.eidos.apps

import java.io.File

import org.clulab.wm.eidoscommon.utils.FileUtils
import org.json4s.DefaultFormats
import org.json4s.JArray
import org.json4s.JNothing
import org.json4s.JObject
import org.json4s.JString
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

object FilterJsonGeoAndTime extends App {

  class Filter() {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    def filter(inputFile: File, jValue: JValue): Unit = {
      println(s"Extracting from ${inputFile.getName}")

      def filterGeo(): Unit = {
        val geoLocations: JValue = (jValue \\ "geolocs" \ "text")

        geoLocations match {
          case JArray(geoLocations: List[_]) => // Type erasure removes the [JString]
            geoLocations.foreach { geoLocation =>
              val text = geoLocation.extract[String]
              val oneLiner = text
                  .replace("\n", "\\n")
                  .replace("\t", "\\t")

              println("\tGeo\t" + oneLiner)
            }
          case JNothing =>
          case _ => throw new RuntimeException(s"Unexpected geoLocations value: $geoLocations")
        }
      }

      def filterTime(): Unit = {
        val timexes: JValue = (jValue \\ "timexes" \ "text")

        timexes match {
          case JArray(timexes: List[_]) => // Type erasure removes the [JString]
            timexes.foreach { timex =>
              val text = timex.extract[String]
              val oneLiner = text
                  .replace("\n", "\\n")
                  .replace("\t", "\\t")

              println("\tTime\t" + oneLiner)
            }
          case JNothing =>
          case _ => throw new RuntimeException(s"Unexpected geoLocations value: $timexes")
        }
      }

      filterGeo()
      filterTime()
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
