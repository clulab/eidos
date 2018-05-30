package org.clulab.wm.eidos.document

import scala.util.Try
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import org.clulab.timenorm.TemporalCharbasedParser
import org.clulab.timenorm.TimeSpan
import org.clulab.anafora.Data
import org.clulab.processors.{Sentence, Document}
import org.clulab.processors.corenlp.CoreNLPDocument

class EidosDocument(sentences: Array[Sentence]) extends CoreNLPDocument(sentences) {

  var time: Array[List[TimeInterval]] = Array()
  def parseTime(timenorm: TemporalCharbasedParser, text:String, dct: Option[String] = None) = {
    val padd = "\n\n\n"
    val dateTime = dct match {
      case Some(date) => Try(LocalDateTime.parse(date + "T00:00:00", DateTimeFormatter.ISO_LOCAL_DATE_TIME)).getOrElse(LocalDateTime.now())
      case None => LocalDateTime.now()
    }
    val anchor = TimeSpan.of(dateTime.getYear, dateTime.getMonthValue, dateTime.getDayOfMonth)
    var prev = 0
    for (s <- this.sentences) {
      time = time :+ (for (i <- timenorm.intervals(timenorm.parse(padd + s.getSentenceText() + padd, anchor))) yield {
        new TimeInterval((i._1._1 + prev, i._1._2 + prev), i._2)
      }).toList
      prev = s.endOffsets.last
    }
  }
}

class TimeInterval(val span: (Int,Int), val intervals: List[(LocalDateTime, Long)])
