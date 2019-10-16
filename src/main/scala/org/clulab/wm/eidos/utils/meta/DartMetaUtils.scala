package org.clulab.wm.eidos.utils.meta

import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.TimeZone

import org.clulab.timenorm.scate.SimpleInterval
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.StringUtils
import org.json4s.DefaultFormats
import org.json4s.JValue
import org.json4s.jackson.JsonMethods.parse
import org.slf4j.Logger
import org.slf4j.LoggerFactory

object DartMetaUtils {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats
  lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  // These are made repeatedly because SimpleDateFormat.parse() is not thread safe or synchronized!
  protected def newDateFormat = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
    val timeZone = TimeZone.getTimeZone("UTC")

    dateFormat.setTimeZone(timeZone)
    dateFormat
  }

  def getDartDocumentTitle(json: Option[JValue]): Option[String] = {
    json.flatMap { json =>
      (json \ "_source" \ "extracted_metadata" \ "Title").extractOpt[String]
    }
  }

  def getDartDocumentLocation(json: Option[JValue]): Option[String] = {
    json.flatMap { json =>
      (json \ "_source" \ "uri").extractOpt[String]
    }
  }

  def getDartDocumentId(json: Option[JValue]): Option[String] = {
    json.flatMap { json =>
      (json \ "_source" \ "document_id").extractOpt[String]
    }
  }

  def getDocumentCreationTime(json: Option[JValue]): Option[DCT] = {

    def toCalendarOpt(json: JValue, label: String): Option[(Calendar, String)] = {
      val longOpt: Option[Long] = (json \ "_source" \ "extracted_metadata" \ label).extractOpt[Long]
      val dateOpt: Option[String] = (json \ "_source" \ "extracted_metadata" \ label).extractOpt[String]
      val calendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"))

      if (longOpt.isDefined) {
        val seconds = longOpt.get
        if (seconds != 0) {
          calendar.setTimeInMillis(seconds * 1000)
          Some(calendar, seconds.toString)
        }
        else
          None
      }
      else if (dateOpt.isDefined) {
        val date = dateOpt.get
        try {
          val parsed = newDateFormat.parse(date)

          calendar.setTime(parsed)
          if (calendar.getTimeInMillis != 0)
            Some(calendar, date)
          else
            None
        }
        catch {
          case throwable: Throwable =>
            logger.error(s"Could not decipher $date as a date", throwable)
            None
        }
      }
      else
        None
    }

    val documentCreationTime = json.flatMap { json =>
      val goodCalendarAndTextOpt: Option[(Calendar, String)] = toCalendarOpt(json, "ModDate")
      val betterCalendarAndTextOpt: Option[(Calendar, String)] = goodCalendarAndTextOpt.orElse(toCalendarOpt(json,"CreationDate"))
      val dctOpt = betterCalendarAndTextOpt.map { case (calendar: Calendar, text: String) =>
        val simpleInterval = SimpleInterval.of(calendar.get(Calendar.YEAR), calendar.get(Calendar.MONTH) + 1, calendar.get(Calendar.DAY_OF_MONTH))
        val dct = DCT(simpleInterval, text)

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
