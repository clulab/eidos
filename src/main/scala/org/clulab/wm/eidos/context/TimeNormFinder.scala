package org.clulab.wm.eidos.context

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin.{Mention, State, TextBoundMention}
import org.clulab.processors.Document
import org.clulab.processors.Sentence
import org.clulab.struct.Interval
import org.clulab.timenorm.neural.{TemporalNeuralParser, TimeExpression}
import org.clulab.wm.eidos.attachments.Time
import org.clulab.wm.eidos.document.{DCT, EidosDocument, TimEx, TimeStep}
import org.clulab.wm.eidos.extraction.Finder
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.Sourcer

import scala.collection.mutable
import scala.collection.mutable
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

  def getTimExs(odinMentions: Seq[Mention], sentences: Array[Sentence]): Array[Seq[TimEx]] = {
    val reachableMentions = EidosMention.findReachableMentions(odinMentions)
    val timExs: Seq[TimEx] = reachableMentions.flatMap { odinMention =>
      odinMention.attachments.collect {
        case attachment: Time => attachment.interval
      }
    }

    var count = 0
    var countSet = 0
    val alignedTimExs: Array[Seq[TimEx]] = sentences.map { sentence =>
      val sentenceStart = sentence.startOffsets.head
      val sentenceEnd = sentence.endOffsets.last

      val result = timExs.filter { timEx =>
        sentenceStart <= timEx.span.start && timEx.span.end <= sentenceEnd
      }

      val resultSet = result.toSet.toSeq
      countSet += resultSet.size

      count += result.size
      result
    }
    println(s"There were $count times found")
    println(s"There were $countSet times in the set found")
    alignedTimExs
  }
}

class TimeNormFinder(parser: TemporalNeuralParser, timeRegexes: Seq[Regex]) extends Finder {
  private val CONTEXT_WINDOW_SIZE = 20
  private val BATCH_SIZE = 40

  override def extract(eidosDocument: Document, initialState: State, dctString: Option[String] = None): Seq[Mention] = {
    // Document must have a text.  There is no other way.
    val Some(text) = eidosDocument.text

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
        // yield the context interval
        Interval(start3, end3)
      }

      // merge overlapping contexts
      val mergedIntervals = singleIntervals.sorted.foldLeft(Seq.empty[Interval]) {
        case (Seq(), context) => Seq(context)
        case (init :+ last, context) if last.end > context.start => init :+ Interval(last.start, context.end)
        case (contexts, context) => contexts :+ context
      }

      // get the text for each context
      for (interval <- mergedIntervals) yield {
        ((sentenceIndex, interval.start), sentenceText.substring(interval.start, interval.end))
      }
    }

    // TODO: TemporalNeuralParser should take Array arguments, not List arguments, so the .toList here can be removed
    val (contextLocations: List[(Int, Int)], contextTexts: List[String]) = sentenceContexts.flatten.toList.unzip

    // run the timenorm parser over batches of contexts to find and normalize time expressions
    val contextTimeExpressions: Seq[List[TimeExpression]] = dctString match {
      case Some(dctString) =>
        val parsed = (dctString :: contextTexts).sliding(BATCH_SIZE, BATCH_SIZE).flatMap(parser.parse).toList
        val dct = Some(DCT(parser.dct(parsed.head), dctString)) // Note the side effect!

        // kwa TODO
//        if (dct.isDefined)
//          eidosDocument.addDocumentAttachment() // kwa TODO

        parser.intervals(parsed.tail, dct.map(_.interval))
      case None =>
        val parsed = contextTexts.sliding(BATCH_SIZE, BATCH_SIZE).flatMap(parser.parse).toList
        parser.intervals(parsed)
    }

    val attachments = new mutable.ArrayBuffer[TimEx]
    // create mentions for each of the time expressions that were found
    val result = for {
      ((sentenceIndex, contextSentenceStart), timeExpressions) <- contextLocations zip contextTimeExpressions
      timeExpression <- timeExpressions
    } yield {
      // reconstruct full-text character offsets from sentence-level character offsets
      val sentence = eidosDocument.sentences(sentenceIndex)
      val contextStart = sentence.startOffsets.head + contextSentenceStart
      val timeTextStart = contextStart + timeExpression.span.start
      val timeTextEnd = contextStart + timeExpression.span.end
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

      // construct the attachment with the detailed time information
      val timeSteps = for (timeInterval <- timeExpression.intervals) yield {
        TimeStep(Option(timeInterval.start), Option(timeInterval.end), timeInterval.duration)
      }
      val attachment = TimEx(timeTextInterval, timeSteps, timeText)
      attachments += attachment

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
    // kwa TODO remove debugging output
    println(s"Subtotal attachments found was ${result.size}")
    println(s"Subtotal attachments found was ${attachments.toArray.toSet.size} in the set")
    result
  }
}
