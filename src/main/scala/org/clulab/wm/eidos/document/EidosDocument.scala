package org.clulab.wm.eidos.document

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.processors.corenlp.CoreNLPDocument
import org.clulab.timenorm.TemporalCharbasedParser
import org.clulab.timenorm.TimeSpan

class EidosDocument(sentences: Array[Sentence], text: Option[String], documentCreationTime: Option[String] = None) extends CoreNLPDocument(sentences) {
  // TODO: @transient here means these values aren't serialized, which sort of defeats the purpose of serialization.
  // Currently no test checks to see if the values are preserved across serialization, but that doesn't make it right.
  @transient val times = new Array[List[TimeInterval]](sentences.length)
  @transient lazy val anchor = {
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
        val sentence_text = text match {
          case Some(t) => t.slice(this.sentences(index).startOffsets(0), this.sentences(index).endOffsets.last)
          case _ => this.sentences(index).getSentenceText()
        }
        val intervals = timenorm.intervals(timenorm.parse(sentence_text, anchor))
        // Sentences use offsets into the document.  Timenorm only knows about the single sentence.
        // Account for this by adding the starting offset of the first word of sentence.
        val offset = this.sentences(index).startOffsets(0)

        // Update  norms with B-I time expressions
        val norms = for (
          ((start, end), norm) <- this.sentences(index).startOffsets zip this.sentences(index).endOffsets zip this.sentences(index).norms.get;
          val inTimex = intervals.map(interval => (start - (interval._1._1 + offset), (interval._1._2 + offset) - end)).filter(x => x._1 >= 0 && x._2 >= 0)
        ) yield {
          inTimex.isEmpty match {
            case false if inTimex(0)._1 == 0 => "B-TMP"
            case false if inTimex(0)._1 != 0 => "I-TMP"
            case _ => norm
          }
        }
        this.sentences(index).norms = Some(norms.toArray)

        intervals.map { interval =>
          new TimeInterval((interval._1._1 + offset, interval._1._2 + offset), interval._2, sentence_text.slice(interval._1._1, interval._1._2))
        }
      }
    }
  }

  def parseTime(timenorm: Option[TemporalCharbasedParser]): Unit =
     if (timenorm.isDefined) parseRealTime(timenorm.get)
     else parseFakeTime()
}

object EidosDocument {

  def apply(document: Document, keepText: Boolean = true, documentCreationTime: Option[String] = None): EidosDocument = {
    val text = document.text // This will be the preprocessed text now.
    // This constructor does not make use of the text,
    val eidosDocument = new EidosDocument(document.sentences, text, documentCreationTime)
    // so it must be set afterwards, if specified.
    if (keepText)
      eidosDocument.text = text
    eidosDocument
  }
}

class TimeInterval(val span: (Int, Int), val intervals: List[(LocalDateTime, Long)], val text: String)
