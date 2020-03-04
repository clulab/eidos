package org.clulab.wm.eidos.apps

import java.io.File
import java.io.PrintWriter
import java.util.regex.Pattern

import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sinker
import org.clulab.wm.eidos.utils.TsvUtils.TsvWriter
import org.json4s.DefaultFormats
import org.json4s.JString
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

object FilterJsonLigatures extends App {
  val pattern: Pattern = Pattern.compile("([A-Za-z]+(f([bhkl]|[ft]|[ij])|ij)) ([A-Za-z]+)")

  class Filter(printWriter: PrintWriter) {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats
    protected val writer = new TsvWriter(printWriter)

    writer.println("file", "left", "right")

    def filter(jValue: JValue, inputFile: File): Unit = {
      val extractions: JValue = jValue \ "_source" \ "extracted_text"

      extractions match {
        case text: JString =>
          val matcher = pattern.matcher(text.extract[String])

          while (matcher.find)
            writer.println(inputFile.getName, matcher.group(1), matcher.group(4))
        case _ => throw new RuntimeException(s"Unexpected extractions value: $extractions")
      }
    }
  }

  val inputDir = args(0)
  val extension = args(1)
  val outputFile = args(2)

  Sinker.printWriterFromFile(outputFile).autoClose { printWriter =>
    val filter = new Filter(printWriter)
    val inputFiles = FileUtils.findFiles(inputDir, extension)

    inputFiles.sortBy(_.getName).foreach { inputFile =>
      val text = FileUtils.getTextFromFile(inputFile)
      val json = JsonMethods.parse(text)

      filter.filter(json, inputFile)
    }
  }
}
