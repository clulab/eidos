package org.clulab.wm.eidos.document

import scala.util.Try
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import org.clulab.timenorm.TemporalCharbasedParser
import org.clulab.timenorm.TimeSpan
import org.clulab.anafora.Data
import org.clulab.processors.{Sentence, Document}
import org.clulab.processors.corenlp.CoreNLPDocument

class EidosDocument(sentences: Array[Sentence], docTime: Option[String] = None) extends CoreNLPDocument(sentences) {

  @transient lazy val dateTime = docTime match {
      case Some(date) => Try(LocalDateTime.parse(date + "T00:00:00", DateTimeFormatter.ISO_LOCAL_DATE_TIME)).getOrElse(LocalDateTime.now())
      case None => LocalDateTime.now()
  }
  @transient lazy val anchor = TimeSpan.of(dateTime.getYear, dateTime.getMonthValue, dateTime.getDayOfMonth)

  var time: Array[List[TimeInterval]] = new Array(sentences.length)
  def parseTime(timenorm: Option[TemporalCharbasedParser], text:String) = {
    var prev = 0
    for ((sent, sidx) <- this.sentences.zipWithIndex) {
      timenorm match {
        case Some(timenorm) => {
          time(sidx) = (for (i <- timenorm.intervals(timenorm.parse(sent.getSentenceText(), this.anchor))) yield {
            new TimeInterval((i._1._1 + prev, i._1._2 + prev), i._2)
          }).toList
        }
        case None => time(sidx) = List[TimeInterval]()
      }
      prev = sent.endOffsets.last
    }
  }
}

class TimeInterval(val span: (Int,Int), val intervals: List[(LocalDateTime, Long)]) extends Serializable


