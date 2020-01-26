package ai.lum.eidos.text

import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.TimeZone

import models.ai.lum.eidos.EidosException
import org.clulab.timenorm.scate.SimpleInterval
import org.clulab.wm.eidos.context.DCT
import org.json4s.JValue
import org.json4s.JsonDSL._

class PlainText(text: String, titleOpt: Option[String] = None, idOpt: Option[String] = None, dateOpt: Option[String] = None, locationOpt: Option[String] = None) extends EidosText {
  protected val dctOpt: Option[DCT] = {
    dateOpt.map { date =>
      val calendar = try {
        val parsed = PlainText.dateFormat.parse(date)
        val calendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"))

        calendar.setTime(parsed)
        calendar
      }
      catch {
        case throwable: Throwable =>
          throw new EidosException(s"""Could not decipher "${date}" as a date""", throwable)
      }

      val simpleInterval = SimpleInterval.of(calendar.get(Calendar.YEAR), calendar.get(Calendar.MONTH) + 1, calendar.get(Calendar.DAY_OF_MONTH))
      DCT(simpleInterval, date)
    }
  }

  override def getText: String = text

  override def getTitleOpt: Option[String] = titleOpt

  override def getIdOpt: Option[String] = idOpt

  override def getDctOpt: Option[DCT] = dctOpt

  override def getLocationOpt: Option[String] = locationOpt

  override def toString: String =
    s"""
       | text = $text
       | title = $titleOpt
       | id = $idOpt
       | dct = ${dctOpt.map(_.interval.start.toString)}
       | location = $locationOpt
       |""".stripMargin

  def toJson: JValue = {
    ("text" -> text) ~
    ("title" -> titleOpt) ~
    ("id" -> idOpt) ~
    ("dct" -> dctOpt.map(_.interval.start.toString)) ~
    ("location" -> locationOpt)
  }
}

object PlainText {
  val dateFormat: SimpleDateFormat = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
    val timeZone = TimeZone.getTimeZone("UTC")

    dateFormat.setTimeZone(timeZone)
    dateFormat
  }
}