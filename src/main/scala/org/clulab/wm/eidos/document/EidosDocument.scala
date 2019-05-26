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

  protected def parseTime(timenorm: TemporalNeuralParser, regexs: List[Regex], documentCreationTime: Option[String]): Array[Seq[TimEx]] = {
    // Only parse text where a regex detects a time expressions. Get the surrounding context. If the contexts for different tokens overlap, join them.
    // Do not include in the context text beyond 2 consecutive newline characters.
    val sentenceText = (sentence: Sentence) => text.map(text => text.slice(sentence.startOffsets.head, sentence.endOffsets.last)).getOrElse(sentence.getSentenceText)
    val sentenceMatches = sentences.map(sentence => regexs.flatMap(_.findAllMatchIn(sentenceText(sentence))).sortBy(_.start))
    val (sentencesToParse, sentenceMatchesToParse) = (sentences zip sentenceMatches).filter{ case(_, m) => m.nonEmpty }.unzip
    val textToParse = sentencesToParse.map(sentenceText).toList

    val nearContext = (sentText: String, match_start: Int, match_end: Int) => {
      val contextStart = {
        val start = max(0, match_start - EidosDocument.CONTEXT_WINDOW_SIZE)
        sentText.slice(start, match_start).reverse.indexOf("\n\n") match {
          case -1 => start
          case i => match_start - i
        }
      }
      val contextEnd = {
        val end = min(match_end + EidosDocument.CONTEXT_WINDOW_SIZE, sentText.length)
        sentText.slice(match_end, end).indexOf("\n\n") match {
          case -1 => end
          case i => match_end + i
        }
      }
      TextInterval(contextStart, contextEnd)
    }
    val spansToParse = sentenceMatchesToParse.zipWithIndex.map{ case(sentMatch, sindex) => sentMatch.map(m => nearContext(textToParse(sindex), m.start, m.end))
    	  .foldLeft(List.empty[TextSpan]) { (list, context) =>
    	     list match {
               case l if l.isEmpty => List(TextSpan(sindex, context))
               case l if l.last.span.end >= context.start => l.init :+ TextSpan(sindex, TextInterval(l.last.span.start, context.end))
               case l if l.last.span.end < context.start => l :+ TextSpan(sindex, context)
               case l => l :+ TextSpan(sindex, context)
    	    }
    	}
    }.toList.flatten
    val contextsToParse = spansToParse.map(spanToParse => textToParse(spanToParse.sentence_id).slice(spanToParse.span.start, spanToParse.span.end))

    // This might be turned into a class with variable names for documentation.
    // The offset might be used in the constructor to adjust it once and for all.
    // Parse the contexts in batches of batch_size. The dct goes in the first batch.
    val contextsTimeExpressions = documentCreationTime match {
      case Some(docTime) =>
          val parsed = (docTime :: contextsToParse).sliding(EidosDocument.BATCH_SIZE, EidosDocument.BATCH_SIZE).flatMap(timenorm.parse).toList
          dct = Some(DCT(timenorm.dct(parsed.head), documentCreationTime.get))
          timenorm.intervals(parsed.tail, Some(dct.get.interval))
      case None => timenorm.intervals(contextsToParse.sliding(EidosDocument.BATCH_SIZE, EidosDocument.BATCH_SIZE).flatMap(timenorm.parse).toList)
    }
    // Recover the list of time expressions for each sentence.
    val docTimeExpressions = (spansToParse zip contextsTimeExpressions).map({ case(spanToParse, timeExpressions) =>
      SentenceTimExs(spanToParse.sentence_id, timeExpressions.map(t =>
        TimExIntervals(TextInterval(t.span.start + spanToParse.span.start, t.span.end + spanToParse.span.start), t.intervals)))
    }).foldLeft(List.empty[SentenceTimExs]) { (list, sTimExs) =>
      list match {
        case l if l.isEmpty => List(sTimExs)
        case l if l.last.sentence_id == sTimExs.sentence_id => l.init :+ SentenceTimExs(l.last.sentence_id, l.last.timexs ::: sTimExs.timexs)
        case l if l.last.sentence_id != sTimExs.sentence_id => l :+ sTimExs
      }
    }
    // Sentences use offsets into the document.  Timenorm only knows about the single sentence.
    // Account for this by adding the offset in time values or subtracting it from word values.
    sentences.map { sentence =>
      if (sentencesToParse.contains(sentence)) {
        val indexOfSentence = sentencesToParse.indexOf(sentence)
        val sentenceText = textToParse(indexOfSentence)
        //val timeExpressions: Seq[TimExIntervals] = docTimeExpressions(indexOfSentence)
        val timeExpressions: SentenceTimExs = docTimeExpressions(indexOfSentence)
        val offset = sentence.startOffsets.head

        // Update norms with B-I time expressions
        sentence.norms.foreach { norms =>
            norms.indices.foreach { index =>
              val wordStart = sentence.startOffsets(index) - offset
              val wordEnd = sentence.endOffsets(index) - offset
              val matchIndex = timeExpressions.timexs.indexWhere { timex =>
                timex.span.start <= wordStart && wordEnd <= timex.span.end
              }
              if (matchIndex >= 0) // word was found inside time expression
                norms(index) =
                    if (wordStart <= timeExpressions.timexs(matchIndex).span.start && wordEnd > timeExpressions.timexs(matchIndex).span.start) "B-Time" // ff wordStart <= timeStart < wordEnd
                    else "I-Time"
          }
        }
        timeExpressions.timexs.map { timex =>
          val timeSteps = timex.intervals.map { interval => TimeStep(Option(interval.start), Option(interval.end), interval.duration) }
          TimEx(TextInterval(timex.span.start + offset, timex.span.end + offset), timeSteps, sentenceText.slice(timex.span.start, timex.span.end))
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
  protected val CONTEXT_WINDOW_SIZE = 20
  protected val BATCH_SIZE = 40

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
case class TextSpan(sentence_id: Int, span: TextInterval)
@SerialVersionUID(1L)
case class TimeStep(startDateOpt: Option[LocalDateTime], endDateOpt: Option[LocalDateTime], duration: Long)
@SerialVersionUID(1L)
case class TimExIntervals(span: TextInterval, intervals: List[TimeInterval])
@SerialVersionUID(1L)
case class SentenceTimExs(sentence_id: Int, timexs: List[TimExIntervals])
@SerialVersionUID(1L)
case class TimEx(span: TextInterval, intervals: List[TimeStep], text: String)
@SerialVersionUID(1L)
case class DCT(interval: TimExInterval, text: String)
