package org.clulab.wm.eidos.apps

import java.io.File
import java.io.PrintWriter

import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sinker
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.TsvWriter
import org.json4s.DefaultFormats
import org.json4s.JArray
import org.json4s.JObject
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

object FilterJsonCanonicalNames extends App {

  class Filter(tsvWriter: TsvWriter) {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    tsvWriter.println("file", "id", "text", "canonicalName")

    def filter(jValue: JValue, inputFile: File): Unit = {
      val extractions: JValue = jValue \\ "extractions"

      extractions match {
        case JArray(extractions: List[_]) => // Type erasure removes the [JObject]
          extractions.foreach { extraction =>
            val id = (extraction \ "@id").extract[String]
            val text = (extraction \ "text").extract[String]
            val canonicalName = (extraction \ "canonicalName").extract[String]

            tsvWriter.println(inputFile.getName, id, text, canonicalName)
          }
        case JObject(_) =>
        case _ => throw new RuntimeException(s"Unexpected extractions value: $extractions")
      }
    }
  }

  val inputDir = args(0)
  val extension = args(1)
  val outputFile = args(2)

  new TsvWriter(Sinker.printWriterFromFile(outputFile)).autoClose { tsvWriter =>
    val filter = new Filter(tsvWriter)
    val inputFiles = FileUtils.findFiles(inputDir, extension)

    inputFiles.sortBy(_.getName).foreach { inputFile =>
      val text = FileUtils.getTextFromFile(inputFile)
      val json = JsonMethods.parse(text)

      filter.filter(json, inputFile)
    }
  }
}
