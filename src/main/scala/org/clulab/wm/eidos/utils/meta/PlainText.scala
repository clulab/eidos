package org.clulab.wm.eidos.utils.meta

import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.TimeZone

import org.clulab.timenorm.scate.SimpleInterval
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.document.Metadata
import org.clulab.wm.eidoscommon.utils.EidosException

class PlainText(text: String,
  titleOpt: Option[String] = None,
  idOpt: Option[String] = None,
  dateOpt: Option[String] = None,
  locationOpt: Option[String] = None
) extends EidosText {
  protected val metadata = {
    val dctOpt: Option[DCT] = {
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

    new Metadata(dctOpt, idOpt, titleOpt, locationOpt)
  }

  def getText: String = text

  def getMetadata: Metadata = metadata
}

object PlainText {
  protected val dateFormat: SimpleDateFormat = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
    val timeZone = TimeZone.getTimeZone("UTC")

    dateFormat.setTimeZone(timeZone)
    dateFormat
  }
}
