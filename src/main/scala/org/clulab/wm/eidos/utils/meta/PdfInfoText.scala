package org.clulab.wm.eidos.utils.meta

import org.clulab.timenorm.scate.SimpleInterval
import org.clulab.wm.eidos.context.DCT
import org.clulab.wm.eidos.document.Metadata
import org.clulab.wm.eidoscommon.EidosException
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.Sourcer
import org.clulab.wm.eidoscommon.utils.StringUtils

import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.TimeZone
import scala.io.Source

class PdfInfoText(text: String, infoOpt: Option[String]) extends EidosText {

  protected val metadata = infoOpt.map { metaText =>
    val lines = metaText.split('\n').map(_.trim)
    val map = lines.map { line =>
      val key = StringUtils.beforeFirst(line, ':', true)
      val value = StringUtils.afterFirst(line, ':', false).trim
      key -> value
    }.toMap
    val dateOpt = map.get("ModDate").orElse(map.get("CreationDate"))
    val dctOpt: Option[DCT] = dateOpt.map { date =>
        val calendar = try {
          val parsed = PdfInfoText.dateFormat.parse(date)
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
    new Metadata(dctOpt, None, None, None)
  }.getOrElse(Metadata())


  def getText: String = text

  def getMetadata: Metadata = metadata
}

object PdfInfoText {
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

  def apply(textFile: File, infoFile: File): PdfInfoText = {
    val text = {
      Sourcer.sourceFromFile(textFile).autoClose { source: Source =>
        source.mkString
      }
    }
    val infoOpt = {
      if (infoFile.exists) {
        Sourcer.sourceFromFile(infoFile).autoClose { source =>
          Some(source.mkString)
        }
      }
      else None
    }

    new PdfInfoText(text, infoOpt)
  }
}
