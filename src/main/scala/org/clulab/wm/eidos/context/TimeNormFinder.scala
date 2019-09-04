package org.clulab.wm.eidos.context

import java.time.LocalDateTime
import java.util.IdentityHashMap

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.anafora.Data
import org.clulab.odin.{Mention, State, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.struct.{Interval => TextInterval}
import org.clulab.timenorm.scate._
import org.clulab.timenorm.scate.{Interval => TimExInterval, Intervals => TimExIntervals}
import org.clulab.timenorm.scate.TemporalNeuralParser
import org.clulab.wm.eidos.attachments.Time
import org.clulab.wm.eidos.document.DctDocumentAttachment
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.Sourcer

import scala.collection.JavaConverters._
import scala.util.Try
import scala.util.control.NonFatal
import scala.util.matching.Regex

@SerialVersionUID(1L)
case class TimeStep(startDate: LocalDateTime, endDate: LocalDateTime)
@SerialVersionUID(1L)
case class TimEx(span: TextInterval, intervals: Seq[TimeStep], text: String)
@SerialVersionUID(1L)
case class DCT(interval: TimExInterval, text: String)

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

  def getTimExs(odinMentions: Seq[Mention]): Seq[TimEx] = {
    val reachableMentions = EidosMention.findReachableMentions(odinMentions)
    val timExSeq: Seq[TimEx] = reachableMentions.flatMap { odinMention =>
      odinMention.attachments.collect {
        case attachment: Time => attachment.interval
      }
    }
    val timExMap: IdentityHashMap[TimEx, Int] = timExSeq.foldLeft(new IdentityHashMap[TimEx, Int]()) { (identityHashMap, timEx) =>
      identityHashMap.put(timEx, 0)
      identityHashMap
    }
    val timExArray = timExMap
        .keySet
        .asScala
        .toArray
        .sortBy { timEx: TimEx => timEx.span }

    timExArray
  }

  def getTimExs(odinMentions: Seq[Mention], sentences: Array[Sentence]): Array[Seq[TimEx]] = {
    val timExs: Seq[TimEx] = getTimExs(odinMentions)
    val alignedTimExs: Array[Seq[TimEx]] = sentences.map { sentence =>
      val sentenceStart = sentence.startOffsets.head
      val sentenceEnd = sentence.endOffsets.last

      timExs.filter { timEx =>
        sentenceStart <= timEx.span.start && timEx.span.end <= sentenceEnd
      }
    }
    alignedTimExs
  }
}

class TimeNormFinder(parser: TemporalNeuralParser, timeRegexes: Seq[Regex]) extends Finder {
  private val CONTEXT_WINDOW_SIZE = 20
  private val BATCH_SIZE = 40

  def getTimExs(odinMentions: Seq[Mention], sentences: Array[Sentence]): Array[Seq[TimEx]] =
      TimeNormFinder.getTimExs(odinMentions, sentences)

  def parseBatch(text: String, spans: Array[(Int, Int)],
                 textCreationTime: TimExInterval = UnknownInterval()): Array[Array[TimeExpression]] = {
    for (xml <- parser.parseBatchToXML(text, spans)) yield {
      implicit val data: Data = new Data(xml, Some(text))
      val reader = new AnaforaReader(textCreationTime)
      data.topEntities.map(e => Try(reader.temporal(e)).getOrElse(UnknownInterval(Some(e.expandedSpan)))).toArray
    }
  }

  def parseDctString(dctString: String): Option[DCT] = {
    Try { // We may be parsing in Javaland and nothing in Eidos seems to be catching, so just in case...
      val dctOpt = parser.parse(dctString).collectFirst { case timExInterval: TimExInterval =>
        DCT(SimpleInterval(timExInterval.start, timExInterval.end), dctString)
      }
      dctOpt
    }.getOrElse(None)
  }

  def find(doc: Document, initialState: State): Seq[Mention] = {
    val Some(text) = doc.text // Document must have a text.
    // extract the pieces of each sentence where we should look for times
    val sentenceContexts = for ((sentence, sentenceIndex) <- doc.sentences.zipWithIndex) yield {
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
        TextInterval(sentenceStart + start3, sentenceStart + end3)
      }

      // merge overlapping contexts
      val mergedIntervals = singleIntervals.sorted.foldLeft(Seq.empty[TextInterval]) {
        case (Seq(), context) => Seq(context)
        case (init :+ last, context) if last.end > context.start => init :+ TextInterval(last.start, context.end)
        case (contexts, context) => contexts :+ context
      }

      // get the text for each context
      for (interval <- mergedIntervals) yield {
        (sentenceIndex, (interval.start, interval.end))
      }
    }
    val (contextSentenceIndexes: Array[Int], contextSpans: Array[(Int, Int)]) = sentenceContexts.flatten.unzip

    // run the timenorm parser over batches of contexts to find and normalize time expressions
    val dctOpt = DctDocumentAttachment.getDct(doc)
    val contextTimeExpressions: Array[Array[TimeExpression]] = dctOpt match {
      case Some(dct: DCT) =>
        contextSpans
            .sliding(BATCH_SIZE, BATCH_SIZE)
            .flatMap(batch => parseBatch(text, batch, textCreationTime = dct.interval))
            .toArray
      case None =>
        contextSpans
            .sliding(BATCH_SIZE, BATCH_SIZE)
            .flatMap(batch => parseBatch(text, batch))
            .toArray
    }

    // create mentions for each of the time expressions that were found
    val mentions = for {
      (sentenceIndex, timeExpressions) <- contextSentenceIndexes zip contextTimeExpressions
      timeExpression <- timeExpressions
    } yield {
      val sentence = doc.sentences(sentenceIndex)
      val Some((timeTextStart, timeTextEnd)) = timeExpression.charSpan
      val timeTextInterval = TextInterval(timeTextStart, timeTextEnd)
      val timeText = text.substring(timeTextStart, timeTextEnd)

      // find the words covered by the time expression (only needed because TextBoundMention requires it)
      val wordIndices = sentence.words.indices.filter { i =>
        sentence.startOffsets(i) < timeTextEnd && timeTextStart < sentence.endOffsets(i)
      }

      // construct a word interval from the word indices
      val wordInterval = if (wordIndices.nonEmpty) {
        TextInterval(wordIndices.head, wordIndices.last + 1)
      } else {

        // the time expression does not overlap with any words, so arbitrarily take the word before it
        val wordIndexBefore = math.max(0, sentence.words.indices.lastIndexWhere(sentence.startOffsets(_) < timeTextEnd))
        TextInterval(wordIndexBefore, wordIndexBefore + 1)
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

      // create the Mention for this time expression
      new TextBoundMention(
        Seq("Time"),
        wordInterval,
        sentenceIndex,
        doc,
        true,
        getClass.getSimpleName,
        Set(Time(attachment))
      )
    }
    mentions
  }
}
