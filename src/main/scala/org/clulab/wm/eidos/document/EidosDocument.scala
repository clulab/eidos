package org.clulab.wm.eidos.document

import java.time.LocalDateTime
import org.clulab.timenorm.TemporalCharbasedParser
import org.clulab.timenorm.TimeSpan
import org.clulab.anafora.Data
import org.clulab.processors.{Sentence, Document}
import org.clulab.processors.corenlp.CoreNLPDocument

class EidosDocument(sentences: Array[Sentence]) extends CoreNLPDocument(sentences) {

  var time: Array[List[TimeInterval]] = Array()
  def parseTime(timenorm: TemporalCharbasedParser, text:String) = {
    val padd = "\n\n\n"
    val now = LocalDateTime.now()
    val anchor = TimeSpan.of(now.getYear, now.getMonthValue, now.getDayOfMonth)
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
