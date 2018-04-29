package org.clulab.wm.eidos.document

import java.time.LocalDate
import org.clulab.timenorm.TemporalCharbasedParser
import org.clulab.timenorm.TimeSpan
import org.clulab.anafora.Data
import org.clulab.processors.{Sentence, Document}
import org.clulab.processors.corenlp.CoreNLPDocument

class EidosDocument(sentences: Array[Sentence]) extends CoreNLPDocument(sentences) {
  var time: Array[Data] = Array()
  def parseTime(timenorm: TemporalCharbasedParser, text:String) = {
    val now = LocalDate.now()
    val anchor = TimeSpan.of(now.getYear, now.getMonthValue, now.getDayOfMonth)
    for (s <- this.sentences)
      time = time :+ timenorm.parse("\n\n\n" + s.getSentenceText() + "\n\n\n", anchor)
  }
}
