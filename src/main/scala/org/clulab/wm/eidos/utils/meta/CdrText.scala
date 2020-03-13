package org.clulab.wm.eidos.utils.meta

import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.TimeZone

import org.clulab.timenorm.scate.SimpleInterval
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.document.Metadata
import org.clulab.wm.eidos.utils.FileUtils
import org.json4s.DefaultFormats
import org.json4s.JObject
import org.json4s.JValue
import org.json4s.jackson.JsonMethods
import org.slf4j.Logger
import org.slf4j.LoggerFactory

class CdrText(cdr: JValue) extends EidosText {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  protected val text: String = (cdr \ "extracted_text").extract[String]
  protected val metadata: Metadata = {
    // It would be nice if this threw an exception on bad data.
    // Check whether keys are there at all and throw exception if parsing returns None.
    val dctOpt: Option[DCT] = getDocumentCreationTime(Some(cdr))
    val locationOpt: Option[String] = getDartDocumentLocation(Some(cdr))
    val titleOpt: Option[String] = getDartDocumentTitle(Some(cdr))
    val idOpt: Option[String] = getDartDocumentId(Some(cdr))

    Metadata(dctOpt, idOpt, titleOpt, locationOpt)
  }

  def getText: String = text

  def getMetadata: Metadata = metadata

  // These are made repeatedly because SimpleDateFormat.parse() is not thread safe or synchronized!
  protected def newDateFormat: SimpleDateFormat = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
    val timeZone = TimeZone.getTimeZone("UTC")

    dateFormat.setTimeZone(timeZone)
    dateFormat
  }

  protected def getDocumentCreationTime(json: Option[JValue]): Option[DCT] = {

    def toCalendarOpt(json: JValue, label: String): Option[(Calendar, String)] = {
      val longOpt: Option[Long] = (json \ "extracted_metadata" \ label).extractOpt[Long]
      val dateOpt: Option[String] = (json \ "extracted_metadata" \ label).extractOpt[String]
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
            CdrText.logger.error(s"Could not decipher $date as a date", throwable)
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

  protected def getDartDocumentLocation(json: Option[JValue]): Option[String] = {
    json.flatMap { json =>
      (json \ "uri").extractOpt[String]
    }
  }

  protected def getDartDocumentTitle(json: Option[JValue]): Option[String] = {
    json.flatMap { json =>
      (json \ "extracted_metadata" \ "Title").extractOpt[String]
    }
  }

  protected def getDartDocumentId(json: Option[JValue]): Option[String] = {
    json.flatMap { json =>
      (json \ "document_id").extractOpt[String]
    }
  }
}

object CdrText {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats
  lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def apply(file: File): CdrText = apply(FileUtils.getTextFromFile(file))

  def apply(json: String): CdrText = {
    val jValue = JsonMethods.parse(json)
    // If there is a _source, it came from an elastic search and the cdr is inside it.
    val cdrValue = (jValue \ "_source").extractOpt[JObject].getOrElse(jValue)

    new CdrText(cdrValue)
  }
}
