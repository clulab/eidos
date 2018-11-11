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
  // Currently no test checks to see if the values are preserved across serialization, but that doesn't make it right.
  val times = new Array[List[TimeInterval]](sentences.length)
  @transient val geolocs = new Array[Seq[GeoPhraseID]](sentences.length)

  protected var anchor: Option[DCT] = None

  protected def parseFakeTime(): Unit = times.indices.foreach(times(_) = List.empty[TimeInterval])
  protected def parseFakeGeoLoc(): Unit = geolocs.indices.foreach(geolocs(_) = Seq.empty[GeoPhraseID])

  protected def parseRealTime(timenorm: TemporalCharbasedParser): Unit = {
    times.indices.foreach { index =>
      val sentence = sentences(index)

      times(index) =
        if (sentence.entities.get.contains("DATE")) {
          val sentence_text = text match {
            case Some(text) => text.slice(sentence.startOffsets(0), sentence.endOffsets.last)
            case _ => sentence.getSentenceText
          }
          val intervals = if (anchor.isDefined)
            timenorm.intervals(timenorm.parse(sentence_text), Some(anchor.get.interval))
          else
            timenorm.intervals(timenorm.parse(sentence_text))
          // Sentences use offsets into the document.  Timenorm only knows about the single sentence.
          // Account for this by adding the starting offset of the first word of sentence.
          val offset = sentence.startOffsets(0)

          // Update norms with B-I time expressions
          val norms = for (
            ((start, end), norm) <- sentence.startOffsets zip sentence.endOffsets zip sentence.norms.get;
            inTimex = intervals.map(interval => (start - (interval._1._1 + offset), (interval._1._2 + offset) - end)).filter(x => x._1 >= 0 && x._2 >= 0)
          ) yield {
            inTimex.isEmpty match {
              case false if inTimex(0)._1 == 0 => "B-Time"
              case false if inTimex(0)._1 != 0 => "I-Time"
              case _ => norm
            }
          }
          sentence.norms = Some(norms.toArray)

          intervals.map { interval =>
            new TimeInterval((interval._1._1 + offset, interval._1._2 + offset), interval._2, sentence_text.slice(interval._1._1, interval._1._2))
          }
        }
        else
          List.empty[TimeInterval]
    }
  }

  def parseDCT(timenorm: Option[TemporalCharbasedParser], documentCreationTime:Option[String]): Unit = {
    if (timenorm.isDefined && documentCreationTime.isDefined)
      anchor = Some(new DCT(timenorm.get.dct(timenorm.get.parse(documentCreationTime.get)), documentCreationTime.get))
  }

  def getDCT(): Option[DCT] = anchor

  def parseTime(timenorm: Option[TemporalCharbasedParser]): Unit =
     if (timenorm.isDefined) parseRealTime(timenorm.get)
     else parseFakeTime()

  def parseGeoNorm(geo_disambiguate: GeoDisambiguateParser): Unit = {
    geolocs.indices.foreach { index =>
      val sentence = sentences(index)

      geolocs(index) = {
        val words = sentence.raw
        val features = geo_disambiguate.makeFeatures(words)
        val labelIndexes = geo_disambiguate.makeLabels(features)
        val norms = sentence.norms.get.zip(labelIndexes).map { case (norm, labelIndex) =>
          if (labelIndex == GeoDisambiguateParser.O_LOC) norm else "LOC"
        }

        sentence.norms = Some(norms) // Updating the norms here
        geo_disambiguate.makeGeoLocations(labelIndexes, words, sentence.startOffsets, sentence.endOffsets)
      }
    }
  }

  def parseGeoNorm_flag(geo_disambiguate: Option[GeoDisambiguateParser]): Unit =
    if (geo_disambiguate.isDefined) parseGeoNorm(geo_disambiguate.get)
    else parseFakeGeoLoc()
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
class TimeInterval(val span: (Int, Int), val intervals: List[(LocalDateTime, LocalDateTime, Long)], val text: String) extends Serializable
@SerialVersionUID(1L)
class DCT(val interval: Interval, val text: String) extends Serializable
