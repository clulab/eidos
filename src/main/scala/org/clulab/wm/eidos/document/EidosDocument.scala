package org.clulab.wm.eidos.document

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import org.clulab.processors.Sentence
import org.clulab.processors.corenlp.CoreNLPDocument
import org.clulab.timenorm.TemporalCharbasedParser
import org.clulab.timenorm.TimeSpan

class EidosDocument(sentences: Array[Sentence], documentCreationTime: Option[String] = None) extends CoreNLPDocument(sentences) {
  val times = new Array[List[TimeInterval]](sentences.length)
  lazy val anchor = {
    val dateTime =
        if (documentCreationTime.isEmpty) LocalDateTime.now()
        // This no longer falls back silently to now().
        else LocalDateTime.parse(documentCreationTime.get + "T00:00:00", DateTimeFormatter.ISO_LOCAL_DATE_TIME)

    TimeSpan.of(dateTime.getYear, dateTime.getMonthValue, dateTime.getDayOfMonth)
  }

  protected def parseFakeTime(): Unit = times.indices.foreach(times(_) = List[TimeInterval]())

  protected def parseRealTime(timenorm: TemporalCharbasedParser): Unit = {
    times.indices.foreach { index =>
      times(index) = {
        val intervals = timenorm.intervals(timenorm.parse(this.sentences(index).getSentenceText(), anchor))
        // Sentences use offsets into the document.  Timenorm only knows about the single sentence.
        // Account for this by adding the starting offset of the first word of sentence.
        val offset = this.sentences(index).startOffsets(0)

        intervals.map { interval =>
          new TimeInterval((interval._1._1 + offset, interval._1._2 + offset), interval._2)
        }
      }
    }
  }

  def parseTime(timenorm: Option[TemporalCharbasedParser]): Unit =
     if (timenorm.isDefined) parseRealTime(timenorm.get)
     else parseFakeTime()
}

class TimeInterval(val span: (Int, Int), val intervals: List[(LocalDateTime, Long)])
