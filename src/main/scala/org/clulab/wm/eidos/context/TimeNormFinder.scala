package org.clulab.wm.eidos.context

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.anafora.Data
import org.clulab.odin.{Mention, State, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.timenorm.formal._
import org.clulab.timenorm.formal.{Interval => TimExInterval, Intervals => TimExIntervals}
import org.clulab.timenorm.neural.TemporalNeuralParser
import org.clulab.wm.eidos.attachments.Time
import org.clulab.wm.eidos.document.{DCT, EidosDocument, TimEx, TimeStep}
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.Sourcer

import scala.collection.mutable
import scala.util.Try
import scala.util.matching.Regex

object TimeNormFinder {
  def fromConfig(config: Config): TimeNormFinder = {
    val timeRegexPath: String = config[String]("timeRegexPath")
    val regexes = Sourcer.sourceFromResource(timeRegexPath).autoClose { source =>
      source
          .getLines
          .map(_.r)
          .toList // rather than toSeq so that source can be closed sooner rather than later
    }
    new TimeNormFinder(new TemporalNeuralParser, regexes)
  }
}

class TimeNormFinder(parser: TemporalNeuralParser, timeRegexes: Seq[Regex]) extends Finder {

  private val CONTEXT_WINDOW_SIZE = 20
  private val BATCH_SIZE = 40

  def parseBatch(text: String, spans: Array[(Int, Int)],
                 textCreationTime: TimExInterval = UnknownInterval()): Array[Array[TimeExpression]] = {
    for (xml <- parser.parseBatchToXML(text, spans)) yield {
      implicit val data: Data = new Data(xml, Some(text))
      val reader = new AnaforaReader(textCreationTime)
      data.topEntities.map(e => Try(reader.temporal(e)).getOrElse(UnknownInterval(Some(e.expandedSpan)))).toArray
    }
  }

  override def extract(doc: Document, initialState: State): Seq[Mention] = doc match {
    case eidosDocument: EidosDocument =>
      val Some(text) = eidosDocument.text

      // TODO: remove EidosDocument.times and just use the Mention objects instead of this
      // pre-initialize the buffers where each sentence's time expressions will be saved
      val sentenceBuffers = eidosDocument.sentences.map(_ => mutable.Buffer.empty[TimEx])
      eidosDocument.times = Some(sentenceBuffers.map(_.toSeq))

      // extract the pieces of each sentence where we should look for times
      val sentenceContexts = for ((sentence, sentenceIndex) <- eidosDocument.sentences.zipWithIndex) yield {
        val sentenceStart = sentence.startOffsets.head
        val sentenceText = text.substring(sentenceStart, sentence.endOffsets.last)

        // find all the places where a regular expression matches
        val matches = timeRegexes.flatMap(_.findAllMatchIn(sentenceText))

        // add some context around each match
        val singleIntervals = for (m <- matches) yield {

          // expand to the specified context window size
          val start1 = math.max(0, m.start - CONTEXT_WINDOW_SIZE)
          val end1 = math.min(sentenceText.length, m.end + CONTEXT_WINDOW_SIZE)
          //assert(0 <= start1 && start1 <= m.start)
          //assert(m.end <= end1 && end1 <= sentenceText.length)

          // do not include context beyond 2 consecutive newline characters
          val separator = "\n\n"
          val start2 = sentenceText.slice(start1, m.start).lastIndexOf(separator) match {
            case -1 => start1
            case i => start1 + i + separator.length
          }
          val end2 = sentenceText.slice(m.end, end1).indexOf(separator) match {
            case -1 => end1
            case i => m.end + i
          }
          //assert(start1 <= start2 && start2 <= m.start)
          //assert(m.end <= end2 && end2 <= end1)

          // expand to word boundaries
          def nextOrElse(iterator: Iterator[Int], x: => Int): Int = if (iterator.hasNext) iterator.next else x
          val start3 = nextOrElse(sentence.startOffsets.reverseIterator.map(_ - sentenceStart).dropWhile(_ > start2), 0)
          val end3 = nextOrElse(sentence.endOffsets.iterator.map(_ - sentenceStart).dropWhile(_ < end2), sentenceText.length)
          //assert(0 <= start3 && start3 <= start2)
          //assert(end2 <= end3 && end3 <= sentenceText.length)

          //assert(start3 <= end3)
          // yield the context interval adjusting the span to full-text character offsets
          Interval(sentenceStart + start3, sentenceStart + end3)
        }

        // merge overlapping contexts
        val mergedIntervals = singleIntervals.sorted.foldLeft(Seq.empty[Interval]) {
          case (Seq(), context) => Seq(context)
          case (init :+ last, context) if last.end > context.start => init :+ Interval(last.start, context.end)
          case (contexts, context) => contexts :+ context
        }

        // get the text for each context
        for (interval <- mergedIntervals) yield {
          (sentenceIndex,(interval.start, interval.end))
        }
      }
      val (contextSentenceIndexes: Array[Int], contextSpans: Array[(Int, Int)]) = sentenceContexts.flatten.unzip

      // run the timenorm parser over batches of contexts to find and normalize time expressions
      val contextTimeExpressions: Array[Array[TimeExpression]] = eidosDocument.dctString match {
        case Some(dctString) =>
          val Array(dct: TimExInterval) = parser.parse(dctString)
          // use a SimpleInterval (character span = None) so it doesn't interfere with character span of TimeExpressions
          val dctSimpleInterval = SimpleInterval(dct.start, dct.end)
          eidosDocument.dct = Some(DCT(dctSimpleInterval, dctString))
          contextSpans.sliding(BATCH_SIZE, BATCH_SIZE).
            flatMap(batch => parseBatch(text, batch, textCreationTime = dctSimpleInterval)).toArray
        case None =>
          contextSpans.sliding(BATCH_SIZE, BATCH_SIZE).
            flatMap(batch => parseBatch(text, batch)).toArray
      }

      // create mentions for each of the time expressions that were found
      for {
        (sentenceIndex, timeExpressions) <- contextSentenceIndexes zip contextTimeExpressions
        timeExpression <- timeExpressions
      } yield {
        val sentence = eidosDocument.sentences(sentenceIndex)
        val Some((timeTextStart, timeTextEnd)) = timeExpression.charSpan
        val timeTextInterval = Interval(timeTextStart, timeTextEnd)
        val timeText = text.substring(timeTextStart, timeTextEnd)

        // find the words covered by the time expression (only needed because TextBoundMention requires it)
        val wordIndices = sentence.words.indices.filter { i =>
          sentence.startOffsets(i) < timeTextEnd && timeTextStart < sentence.endOffsets(i)
        }

        // construct a word interval from the word indices
        val wordInterval = if (wordIndices.nonEmpty) {
          Interval(wordIndices.head, wordIndices.last + 1)
        } else {

          // the time expression does not overlap with any words, so arbitrarily take the word before it
          val wordIndexBefore = math.max(0, sentence.words.indices.lastIndexWhere(sentence.startOffsets(_) < timeTextEnd))
          Interval(wordIndexBefore, wordIndexBefore + 1)
        }

        // get the Seq of Intervals for each TimeExpression and construct the attachment with the detailed time information
        val timeSteps: Seq[TimeStep] = timeExpression match {
          case timeInterval: TimExInterval if timeInterval.isDefined =>
            Seq(TimeStep(timeInterval.start, timeInterval.end))
          case timeIntervals: TimExIntervals if timeIntervals.isDefined =>
            timeIntervals.iterator.toSeq.map(interval => TimeStep(interval.start, interval.end))
          case _ => Seq()
        }
        val attachment = TimEx(timeTextInterval, timeSteps, timeText)

        // add the attachment directly to the document, since that's currently required as well
        sentenceBuffers(sentenceIndex) += attachment

        // create the Mention for this time expression
        new TextBoundMention(
          Seq("Time"),
          wordInterval,
          sentenceIndex,
          eidosDocument,
          true,
          getClass.getSimpleName,
          Set(Time(attachment))
        )
      }
  }
}
