package org.clulab.wm.eidos.apps.batch

import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.{FileUtils, Logging}
import org.json4s.DefaultFormats
import org.json4s.{JArray, JField, JObject, JString, JValue}
import org.json4s.jackson.JsonMethods

object ExtractCausationFromDirectory extends App with Logging {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  val inputDir = args(0)
  val outputFile = args(1)

  val files = FileUtils.findFiles(inputDir, "jsonld")

  FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
//    printWriter.println(s"file\tcausation")
    printWriter.println("causation")
    files.foreach { file =>
      try {
        val text = FileUtils.getTextFromFile(file)
        val jValue = JsonMethods.parse(text)
        val jObject = jValue.extract[JObject]
        val extractions = (jObject \ "extractions").extractOpt[JArray]
            .map(_.arr)
            .getOrElse(List.empty[JValue])
        val causations = extractions.filter { extraction =>
          (extraction \ "subtype").extract[String] == "causation"
        }
        val texts = causations.map { causation =>
          (causation \ "text").extract[String]
        }

        texts.foreach { text =>
//          printWriter.println(s"${file.getName}\t$text")
          printWriter.println(text)
        }
      }
      catch {
        case exception: Exception =>
          logger.error(s"Exception for file $file", exception)
      }
    }
  }
}
