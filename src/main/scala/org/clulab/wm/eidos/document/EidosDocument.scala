package org.clulab.wm.eidos.document

import java.time.LocalDateTime

import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.processors.corenlp.CoreNLPDocument
import org.clulab.timenorm.TemporalCharbasedParser
import org.clulab.timenorm.formal.Interval
import org.clulab.wm.eidos.context.GeoDisambiguateParser
import org.clulab.wm.eidos.context.GeoPhraseID

class EidosDocument(sentences: Array[Sentence], text: Option[String]) extends CoreNLPDocument(sentences) {
  // At some point these will turn into Array[Option[Seq[...]]].  Each sentences will have its own
  // Option[Seq[...]] as they do other things like tags, entities, and lemmas.
  var times: Option[Array[Seq[TimeInterval]]] = None
  var geolocs: Option[Array[Seq[GeoPhraseID]]] = None
  var dct: Option[DCT] = None

  protected def parseTime(timenorm: TemporalCharbasedParser): Array[Seq[TimeInterval]] = {
    sentences.map { sentence =>
      if (sentence.entities.get.contains("DATE")) {
        val sentenceText = text
            .map(text => text.slice(sentence.startOffsets(0), sentence.endOffsets.last))
            .getOrElse(sentence.getSentenceText)
        // This might be turned into a class with variable names for documentation.
        // The offset might be used in the constructor to adjust it once and for all.
        val intervals: Seq[((Int, Int), List[(LocalDateTime, LocalDateTime, Long)])] = dct
            .map(dct => timenorm.intervals(timenorm.parse(sentenceText), Some(dct.interval)))
            .getOrElse(timenorm.intervals(timenorm.parse(sentenceText)))
        // Sentences use offsets into the document.  Timenorm only knows about the single sentence.
        // Account for this by adding the offset in time values or subtracting it from word values.
        val offset = sentence.startOffsets(0)

        // Update norms with B-I time expressions
        sentence.norms.foreach { norms =>
            norms.indices.foreach { index =>
              val wordStart = sentence.startOffsets(index) - offset
              val wordEnd = sentence.endOffsets(index) - offset
              val matchIndex = intervals.indexWhere { interval =>
                val timeStart = interval._1._1
                val timeEnd = interval._1._2

                timeStart <= wordStart && wordEnd <= timeEnd
              }
              if (matchIndex >= 0) // word was found inside time expression
                norms(index) =
                    if (wordStart == intervals(matchIndex)._1._1) "B-Time" // ff wordStart == timeStart
                    else "I-Time"
          }
        }
        intervals.map { interval =>
          TimeInterval((interval._1._1 + offset, interval._1._2 + offset), interval._2, sentenceText.slice(interval._1._1, interval._1._2))
        }
      }
      else
        Seq.empty[TimeInterval]
    }
  }

  def parseDCT(timenorm: Option[TemporalCharbasedParser], documentCreationTime:Option[String]): Unit = {
    if (timenorm.isDefined && documentCreationTime.isDefined)
      dct = Some(DCT(timenorm.get.dct(timenorm.get.parse(documentCreationTime.get)), documentCreationTime.get))
  }

  def parseTime(timenorm: Option[TemporalCharbasedParser]): Unit =
     times = timenorm.map(parseTime)

  def parseGeoNorm(geoDisambiguateParser: GeoDisambiguateParser): Array[Seq[GeoPhraseID]] = {
    sentences.map { sentence =>
      val words = sentence.raw
      val features = geoDisambiguateParser.makeFeatures(words)
      val labelIndexes = geoDisambiguateParser.makeLabels(features)

      // Update norms with LOC, no B-LOC or I-LOC used
      sentence.norms.foreach { norms =>
        norms.indices.foreach { index =>
            if (labelIndexes(index) != GeoDisambiguateParser.O_LOC)
              norms(index) = "LOC"
        }
      }
      geoDisambiguateParser.makeGeoLocations(labelIndexes, words, sentence.startOffsets, sentence.endOffsets)
    }
  }

  def parseGeoNorm(geoDisambiguateParser: Option[GeoDisambiguateParser]): Unit =
      geolocs = geoDisambiguateParser.map(parseGeoNorm)
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
case class TimeInterval(span: (Int, Int), intervals: List[(LocalDateTime, LocalDateTime, Long)], text: String)
@SerialVersionUID(1L)
case class DCT(interval: Interval, text: String)
