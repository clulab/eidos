package org.clulab.wm.eidos.document

import java.time.LocalDateTime

import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.processors.corenlp.CoreNLPDocument
import org.clulab.timenorm.neural.TimeInterval
import org.clulab.timenorm.formal.{Interval => TimExInterval}
import org.clulab.struct.{Interval => TextInterval}
import org.clulab.wm.eidos.context.GeoPhraseID

class EidosDocument(sentences: Array[Sentence], text: Option[String]) extends CoreNLPDocument(sentences) {
  // At some point these will turn into Array[Option[Seq[...]]].  Each sentences will have its own
  // Option[Seq[...]] as they do other things like tags, entities, and lemmas.
  var times: Option[Array[Seq[TimEx]]] = None
  var geolocs: Option[Array[Seq[GeoPhraseID]]] = None
  var dct: Option[DCT] = None
  var dctString: Option[String] = None
}

object EidosDocument {

  def apply(document: Document, keepText: Boolean = true): EidosDocument = {
    val text = document.text // This will be the preprocessed text now.
    // This constructor does not make use of the text,
    val eidosDocument = new EidosDocument(document.sentences, text)
    // so it must be set afterwards, if specified.
    if (keepText)
      eidosDocument.text = text
    eidosDocument
  }
}

@SerialVersionUID(1L)
case class TimeStep(startDateOpt: Option[LocalDateTime], endDateOpt: Option[LocalDateTime], duration: Long)
@SerialVersionUID(1L)
case class TimEx(span: TextInterval, intervals: Seq[TimeStep], text: String)
@SerialVersionUID(1L)
case class DCT(interval: TimExInterval, text: String)

@SerialVersionUID(1L)
case class SentenceIndexAndTextInterval(sentenceIndex: Int, textInterval: TextInterval)
@SerialVersionUID(1L)
case class TextIntervalAndTimeIntervals(textInterval: TextInterval, timeIntervals: Seq[TimeInterval])
@SerialVersionUID(1L)
case class SentenceIndexAndTextIntervalAndTimeIntervals(sentenceIndex: Int, multiTextIntervalAndTimeIntervals: Seq[TextIntervalAndTimeIntervals])

@SerialVersionUID(1L)
case class SentenceBundle(index: Int, start: Int, sentence: Sentence, text: String, multiSentenceIndexAndTextInterval: Seq[SentenceIndexAndTextInterval], contexts: Seq[String])
