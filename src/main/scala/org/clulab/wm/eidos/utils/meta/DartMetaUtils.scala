package org.clulab.wm.eidos.utils.meta

import java.io.File
import java.util.Calendar
import java.util.TimeZone

import org.clulab.timenorm.scate.SimpleInterval
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.StringUtils
import org.json4s.DefaultFormats
import org.json4s.JValue
import org.json4s.jackson.JsonMethods.parse

object DartMetaUtils {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  def getDartDocumentTitle(json: Option[JValue]): Option[String] = {
    json.flatMap { json =>
      Option((json \ "_source" \ "extracted_metadata" \ "Title").extract[String])
    }
  }

  def getDartDocumentLocation(json: Option[JValue]): Option[String] = {
    json.flatMap { json =>
      Option((json \ "_source" \ "uri").extract[String])
    }
  }

  def getDartDocumentId(json: Option[JValue]): Option[String] = {
    json.flatMap { json =>
      Option((json \ "_source" \ "document_id").extract[String])
    }
  }

  def getDocumentCreationTime(json: Option[JValue]): Option[DCT] = {

    val documentCreationTime = json.flatMap { json =>
      val goodDateOpt: Option[Long] = {
        val date: Long = (json \ "_source" \ "extracted_metadata" \ "ModDate").extract[Long]

        if (date == 0) None
        else Some(date)
      }
      val okDateOpt: Option[Long] = goodDateOpt.orElse {
        val date: Long = (json \ "_source" \ "extracted_metadata" \ "CreationDate").extract[Long]

        if (date == 0) None
        else Some(date)
      }
      val dctOpt = okDateOpt.map { seconds =>
        val calendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
        calendar.setTimeInMillis(seconds * 1000)
        val simpleInterval = SimpleInterval.of(calendar.get(Calendar.YEAR), calendar.get(Calendar.MONTH) + 1, calendar.get(Calendar.DAY_OF_MONTH))
        val dct = DCT(simpleInterval, seconds.toString)

        dct
      }

      dctOpt
    }
    documentCreationTime
  }

  def getMetaData(metaFile: File): Option[JValue] = {
    val json = if (metaFile.exists()) {
      val text = FileUtils.getTextFromFile(metaFile)
      val json = parse(text)

      Some(json)
    }
    else None

    json
  }

  def getMetaData(converter: (String, File) => File, metaDir: String, textFile: File): Option[JValue] = {
    val metaFile = converter(metaDir, textFile)

    getMetaData(metaFile)
  }

  def convertTextToMeta(metaDir: String, textFile: File): File = {
    val textFileName = textFile.getName
    val metaFileName = metaDir + "/" + StringUtils.beforeLast(textFileName, '.') + ".json"

    new File(metaFileName)
  }

  def convertTextToJsonld(jsonldDir: String, textFile: File): File = {
    val textFileName = textFile.getName
    val jsonldFileName = jsonldDir + "/" + StringUtils.beforeLast(textFileName, '.') + ".jsonld"

    new File(jsonldFileName)
  }
}
