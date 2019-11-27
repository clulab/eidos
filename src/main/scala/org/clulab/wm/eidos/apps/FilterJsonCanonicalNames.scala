package org.clulab.wm.eidos.apps

import java.io.File
import java.io.PrintWriter

import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.utils.Sinker
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.json4s.DefaultFormats
import org.json4s.JArray
import org.json4s.JObject
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

object FilterJsonCanonicalNames extends App {

  class Filter(printWriter: PrintWriter) {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    printWriter.println("file\tid\ttext\tcanonicalName")

    def escape(text: String): String = {
      text
          .replace("\\", "\\\\")
          .replace("\n", "\\n")
          .replace("\r", "\\r")
          .replace("\t", "\\t")
    }

    def filter(inputFile: File, jValue: JValue): Unit = {
      val extractions: JValue = jValue \\ "extractions"

      extractions match {
        case JArray(extractions: List[_]) => // Type erasure removes the [JObject]
          extractions.foreach { extraction =>
            val id = (extraction \ "@id").extract[String]
            val text = (extraction \ "text").extract[String]
            val canonicalName = (extraction \ "canonicalName").extract[String]

            printWriter.println(s"${inputFile.getName}\t$id\t${escape(text)}\t${escape(canonicalName)}")
          }
        case JObject(_) =>
        case _ => throw new RuntimeException(s"Unexpected extractions value: $extractions")
      }
    }
  }

  val inputDir = args(0)
  val extension = args(1)
  val outputFile = args(2)

  Sinker.printWriterFromFile(outputFile).autoClose { printWriter =>
    val filter = new Filter(printWriter)
    val inputFiles = findFiles(inputDir, extension)

    inputFiles.foreach { inputFile =>
      val text = FileUtils.getTextFromFile(inputFile)
      val json = JsonMethods.parse(text)

      filter.filter(inputFile, json)
    }
  }
}
