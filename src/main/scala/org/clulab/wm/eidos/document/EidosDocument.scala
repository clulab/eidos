package org.clulab.wm.eidos.document

import java.time.LocalDateTime

import scala.math.{max, min}
import scala.util.matching.Regex
import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.processors.corenlp.CoreNLPDocument
import org.clulab.timenorm.neural.{TemporalNeuralParser, TimeInterval}
import org.clulab.timenorm.formal.{ Interval => TimExInterval }
import org.clulab.struct.{ Interval => TextInterval }
import org.clulab.wm.eidos.context.GeoDisambiguateParser
import org.clulab.wm.eidos.context.GeoPhraseID


class EidosDocument(sentences: Array[Sentence], text: Option[String]) extends CoreNLPDocument(sentences) {
  // At some point these will turn into Array[Option[Seq[...]]].  Each sentences will have its own
  // Option[Seq[...]] as they do other things like tags, entities, and lemmas.
  var times: Option[Array[Seq[TimEx]]] = None
  var geolocs: Option[Array[Seq[GeoPhraseID]]] = None
  var dct: Option[DCT] = None
  var context_window_size = 50
  var batch_size = 40
  type TimExType = ((Int, Int), List[TimeInterval])

  protected def parseTime(timenorm: TemporalNeuralParser, regexs: List[Regex], documentCreationTime: Option[String]): Array[Seq[TimEx]] = {
    // Only parse text where a regex detects a time expressions. Get the surrounding context. If the contexts for different tokens overlap, join them.
    // Do not include in the context text beyond 2 consecutive newline characters.
    val sentenceText = (sentence: Sentence) => text.map(text => text.slice(sentence.startOffsets.head, sentence.endOffsets.last)).getOrElse(sentence.getSentenceText)
    val sentenceMatches = sentences.map(sentence => regexs.map(_.findAllMatchIn(sentenceText(sentence)).toList).flatten.sortBy(_.start))
    val (sentencesToParse, sentenceMatchesToParse) = (sentences zip sentenceMatches).filter{ case(_, m) => m.nonEmpty }.unzip
    val textToParse = sentencesToParse.map(sentenceText).toList

    val nearContext = (sentText: String, match_start: Int, match_end: Int) => {
      val contextStart = {
        val start = max(0, match_start - context_window_size)
        sentText.slice(start, match_start).reverse.indexOf("\n\n") match {
          case -1 => start
          case i => match_start - i
        }
      }
      val contextEnd = {
        val end = min(match_end + context_window_size, sentText.length)
        sentText.slice(match_end, end).indexOf("\n\n") match {
          case -1 => end
          case i => match_end + i
        }
      }
      (contextStart, contextEnd)
    }
    val spansToParse = sentenceMatchesToParse
        .zipWithIndex
        .map { case(sentMatch, sindex) => 
          sentMatch
              .map(m => nearContext(textToParse(sindex), m.start, m.end))
              .foldLeft(List.empty[(Int, Int, Int)]) { (list, c) =>
                list match {
                  case l if l.isEmpty => List((sindex, c._1, c._2))
                  case l if l.last._3 >= c._1 => l.init :+ (sindex, l.last._2, c._2)
                  case l if l.last._3 < c._1 => l :+ (sindex, c._1, c._2)
                  case l => l :+ (sindex, c._1, c._2)
                }
              }
        }.toList.flatten
    val contextsToParse = spansToParse.map(span => textToParse(span._1).slice(span._2, span._3))

    // This might be turned into a class with variable names for documentation.
    // The offset might be used in the constructor to adjust it once and for all.
    // Parse the contexts in batches of batch_size. The dct goes in the first batch.
    val contextsTimeExpressions = documentCreationTime match {
      case Some(docTime) =>
          val parsed = (docTime :: contextsToParse).sliding(batch_size, batch_size).flatMap(timenorm.parse).toList
          dct = Some(DCT(timenorm.dct(parsed.head), documentCreationTime.get))
          timenorm.intervals(parsed.tail, Some(dct.get.interval))
      case None => timenorm.intervals(contextsToParse.sliding(batch_size, batch_size).flatMap(timenorm.parse).toList)
    }
    // Recover the list of intervals for each sentence.
    val docTimeExpressions = (spansToParse zip contextsTimeExpressions).map({ case(span, timeExpressions) =>
      (span._1, timeExpressions.map(t => ((t.span.start + span._2, t.span.end + span._2), t.intervals)))
    }).foldLeft(List.empty[(Int, List[TimExType])]) { (list, n) =>
      list match {
        case l if l.isEmpty => List(n)
        case l if l.last._1 == n._1 => l.init :+ (l.last._1, l.last._2 ::: n._2)
        case l if l.last._1 != n._1 => l :+ n
      }
    }.map(_._2)
    // Sentences use offsets into the document.  Timenorm only knows about the single sentence.
    // Account for this by adding the offset in time values or subtracting it from word values.
    sentences.map { sentence =>
      if (sentencesToParse.contains(sentence)) {
        val indexOfSentence = sentencesToParse.indexOf(sentence)
        val sentenceText = textToParse(indexOfSentence)
        val timeExpressions: Seq[TimExType] = docTimeExpressions(indexOfSentence)
        val offset = sentence.startOffsets.head

        // Update norms with B-I time expressions
        sentence.norms.foreach { norms =>
            norms.indices.foreach { index =>
              val wordStart = sentence.startOffsets(index) - offset
              val wordEnd = sentence.endOffsets(index) - offset
              val matchIndex = timeExpressions.indexWhere { timex =>
                val timeStart = timex._1._1
                val timeEnd = timex._1._2

                timeStart <= wordStart && wordEnd <= timeEnd
              }
              if (matchIndex >= 0) // word was found inside time expression
                norms(index) =
                    if (wordStart == timeExpressions(matchIndex)._1._1) "B-Time" // ff wordStart == timeStart
                    else "I-Time"
          }
        }
        timeExpressions.map { timex =>
          val timeSteps = timex._2.map { interval => TimeStep(Option(interval.start), Option(interval.end), interval.duration) }
          TimEx(TextInterval(timex._1._1 + offset, timex._1._2 + offset), timeSteps, sentenceText.slice(timex._1._1, timex._1._2))
        }
      }
      else
        Seq.empty[TimEx]
    }
  }

  def parseTime(timenorm: Option[TemporalNeuralParser], regexs: Option[List[Regex]], documentCreationTime: Option[String]): Unit =
     times = timenorm.map(parseTime(_, regexs.get, documentCreationTime))

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
case class TimeStep(startDateOpt: Option[LocalDateTime], endDateOpt: Option[LocalDateTime], duration: Long)
@SerialVersionUID(1L)
case class TimEx(span: TextInterval, intervals: List[TimeStep], text: String)
@SerialVersionUID(1L)
case class DCT(interval: TimExInterval, text: String)
