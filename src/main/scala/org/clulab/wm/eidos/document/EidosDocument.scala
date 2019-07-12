package org.clulab.wm.eidos.document

import java.time.LocalDateTime

import scala.util.matching.Regex
import org.clulab.processors.Document
import org.clulab.processors.DocumentAttachment
import org.clulab.processors.DocumentAttachmentBuilderFromJson
import org.clulab.processors.DocumentAttachmentBuilderFromText
import org.clulab.processors.Sentence
import org.clulab.timenorm.neural.{TemporalNeuralParser, TimeInterval}
import org.clulab.timenorm.formal.{Interval => TimExInterval}
import org.clulab.struct.{Interval => TextInterval}
import org.clulab.timenorm.formal.SimpleInterval
import org.clulab.timenorm.formal.UnknownInterval
import org.clulab.timenorm.neural.TimeExpression
import org.clulab.wm.eidos.context.GeoPhraseID
import org.json4s._
import org.json4s.JsonDSL._

class EidosDocument(sentences: Array[Sentence], text: Option[String]) extends Document(sentences) {
  // At some point these will turn into Array[Option[Seq[...]]].  Each sentences will have its own
  // Option[Seq[...]] as they do other things like tags, entities, and lemmas.
  var times: Option[Array[Seq[TimEx]]] = None
  var geolocs: Option[Array[Seq[GeoPhraseID]]] = None
//  var dct: Option[DCT] = None

  protected def mkNearContext(sentenceText: String, matchStart: Int, matchEnd: Int): TextInterval = {
    // Do not include in the context text beyond 2 consecutive newline characters.
    val separator = "\n\n"
    val contextStart = {
      val start = math.max(0, matchStart - EidosDocument.CONTEXT_WINDOW_SIZE)
      sentenceText.slice(start, matchStart).reverse.indexOf(separator) match {
        case -1 => start
        case i => matchStart - i
      }
    }
    val contextEnd = {
      val end = math.min(matchEnd + EidosDocument.CONTEXT_WINDOW_SIZE, sentenceText.length)
      sentenceText.slice(matchEnd, end).indexOf(separator) match {
        case -1 => end
        case i => matchEnd + i
      }
    }

    TextInterval(contextStart, contextEnd)
  }

  protected def getSentenceText(sentence: Sentence): String = {
    text.map(text => text.slice(sentence.startOffsets.head, sentence.endOffsets.last))
        .getOrElse(sentence.getSentenceText)
  }

  protected def mkMultiSentenceIndexAndTextInterval(sentenceIndex: Int, sentenceText: String, matches: Seq[Regex.Match]): Seq[SentenceIndexAndTextInterval] = {
    val separateContexts: Seq[TextInterval] = matches.map(`match` => mkNearContext(sentenceText, `match`.start, `match`.end))
    val joinedContexts: Seq[SentenceIndexAndTextInterval] = separateContexts.foldLeft(Seq.empty[SentenceIndexAndTextInterval]) { (list, context) =>
      list match { // This name change it to keep IntelliJ from complaining
        case li if li.isEmpty => List(SentenceIndexAndTextInterval(sentenceIndex, context))
        case li if li.last.textInterval.end >= context.start => li.init :+ SentenceIndexAndTextInterval(sentenceIndex, TextInterval(li.last.textInterval.start, context.end))
        case li => li :+ SentenceIndexAndTextInterval(sentenceIndex, context)
      }
    }

    joinedContexts
  }

  protected def mkMultiSentenceIndexAndTextIntervalAndTimeIntervals(textIntervalsToParse: List[SentenceIndexAndTextInterval],
      contextsTimeExpressions: Seq[List[TimeExpression]], sentenceBundles: Seq[SentenceBundle]):
      Seq[SentenceIndexAndTextIntervalAndTimeIntervals] = {
    val separateExpressions: Seq[SentenceIndexAndTextIntervalAndTimeIntervals] = (textIntervalsToParse zip contextsTimeExpressions).map { case (textSpanToParse, contextTimeExpressions) =>
      val sentenceStart = sentenceBundles(textSpanToParse.sentenceIndex).start
      val intervals: Seq[TextIntervalAndTimeIntervals] = contextTimeExpressions.map { contextTimeExpression =>
        TextIntervalAndTimeIntervals(
          TextInterval(
            // This is now the entire correction for location in the sentence and document.
            sentenceStart + textSpanToParse.textInterval.start + contextTimeExpression.span.start,
            sentenceStart + textSpanToParse.textInterval.start + contextTimeExpression.span.end
          ),
          contextTimeExpression.intervals
        )
      }
      SentenceIndexAndTextIntervalAndTimeIntervals(textSpanToParse.sentenceIndex, intervals)
    }
    val joinedExpressions: Seq[SentenceIndexAndTextIntervalAndTimeIntervals] = separateExpressions.foldLeft(List.empty[SentenceIndexAndTextIntervalAndTimeIntervals]) { (list, sTimExs) =>
      list match {
        case li if li.isEmpty => List(sTimExs)
        case li if li.last.sentenceIndex == sTimExs.sentenceIndex => li.init :+ SentenceIndexAndTextIntervalAndTimeIntervals(li.last.sentenceIndex, li.last.multiTextIntervalAndTimeIntervals ++: sTimExs.multiTextIntervalAndTimeIntervals)
        case li if li.last.sentenceIndex != sTimExs.sentenceIndex => li :+ sTimExs
      }
    }

    joinedExpressions
  }

  protected def mkTimExs(sentenceBundle: SentenceBundle, multiTextIntervalAndTimeIntervals: Seq[TextIntervalAndTimeIntervals]): Seq[TimEx] = {
    multiTextIntervalAndTimeIntervals.map { textIntervalAndTimeIntervals: TextIntervalAndTimeIntervals =>
      val timeSteps: Seq[TimeStep] = textIntervalAndTimeIntervals.timeIntervals.map { timeInterval: TimeInterval =>
        TimeStep(Option(timeInterval.start), Option(timeInterval.end), timeInterval.duration)
      }
      val text = sentenceBundle.text.slice(textIntervalAndTimeIntervals.textInterval.start - sentenceBundle.start,
        textIntervalAndTimeIntervals.textInterval.end - sentenceBundle.start)

      TimEx(textIntervalAndTimeIntervals.textInterval, timeSteps, text)
    }
  }

  protected def updateNorms(sentenceBundle: SentenceBundle, multiTextIntervalAndTimeIntervals: Seq[TextIntervalAndTimeIntervals]): Unit = {
    val sentence = sentenceBundle.sentence

    // TODO: This might need to change to deal with overlapping TextIntervals.  There should be an
    // issue filed against the problem.
    // Update norms with B-I time expressions
    sentence.norms.foreach { norms =>
      norms.indices.foreach { index =>
        val wordStart = sentence.startOffsets(index)
        val wordEnd = sentence.endOffsets(index)
        // This only finds the first one, but there may be more.
        val matchIndex = multiTextIntervalAndTimeIntervals.indexWhere { textIntervalAndTimeIntervals =>
          textIntervalAndTimeIntervals.textInterval.start <= wordStart &&
              wordEnd <= textIntervalAndTimeIntervals.textInterval.end
        }

        if (matchIndex >= 0) // word was found inside time expression
          norms(index) =
              if (wordStart <= multiTextIntervalAndTimeIntervals(matchIndex).textInterval.start &&
                  multiTextIntervalAndTimeIntervals(matchIndex).textInterval.start < wordEnd) "B-Time"
              else "I-Time"
      }
    }
  }

  protected def parseTime(timenorm: TemporalNeuralParser, regexs: List[Regex], docTimeOpt: Option[String]): Array[Seq[TimEx]] = {
    // Bundle up everything about a sentence that is needed later.
    val sentenceBundles: Array[SentenceBundle] = sentences.zipWithIndex.map { case (sentence, index) =>
      val text: String = getSentenceText(sentence)
      // Only parse text where a regex detects a time expressions. Get the surrounding context.
      val matches: Seq[Regex.Match] = regexs.flatMap(_.findAllMatchIn(text)).sortBy(_.start)
      // If the contexts for different tokens overlap, join them.
      val multiSentenceIndexAndTextInterval: Seq[SentenceIndexAndTextInterval] = mkMultiSentenceIndexAndTextInterval(index, text, matches)
      val contexts: Seq[String] = multiSentenceIndexAndTextInterval.map { sentenceIndexAndTextInterval =>
        text.slice(sentenceIndexAndTextInterval.textInterval.start, sentenceIndexAndTextInterval.textInterval.end)
      }

      SentenceBundle(index, sentence.startOffsets.head, sentence, text, multiSentenceIndexAndTextInterval, contexts)
    }
    // These next two lists work in parallel, allowing timeExpressions to be indexed to the sentences.
    val (multiSentenceIndexAndTextInterval, contexts) = {
      val listSentenceBundles = sentenceBundles.toList

      (listSentenceBundles.flatMap(_.multiSentenceIndexAndTextInterval), listSentenceBundles.flatMap(_.contexts))
    }
    // Parse the contexts in batches of batch_size. The dct goes in the first batch.
    val timeExpressions: Seq[List[TimeExpression]] = docTimeOpt match {
      case Some(docTime) =>
        val parsed = (docTime :: contexts).sliding(EidosDocument.BATCH_SIZE, EidosDocument.BATCH_SIZE).flatMap(timenorm.parse).toList
        val dct = DCT(timenorm.dct(parsed.head), docTime) // Note the side effect!
        this.addAttachment("dct", new DctDocumentAttachment(dct))
        timenorm.intervals(parsed.tail, Option(dct.interval))
      case None =>
        val parsed = contexts.sliding(EidosDocument.BATCH_SIZE, EidosDocument.BATCH_SIZE).flatMap(timenorm.parse).toList
        timenorm.intervals(parsed)
    }
    // Correct offsets and combine them on a per sentence basis.
    val timExMap = mkMultiSentenceIndexAndTextIntervalAndTimeIntervals(multiSentenceIndexAndTextInterval, timeExpressions, sentenceBundles)
        // Recover the list of time expressions for each sentence.
        .groupBy(_.sentenceIndex)
        .map { case (sentenceIndex, multiSentenceIndexAndTextIntervalAndTimeIntervals) =>
          val multiTextIntervalAndTimeIntervals = multiSentenceIndexAndTextIntervalAndTimeIntervals.flatMap(_.multiTextIntervalAndTimeIntervals)

          updateNorms(sentenceBundles(sentenceIndex), multiTextIntervalAndTimeIntervals)
          sentenceIndex -> mkTimExs(sentenceBundles(sentenceIndex), multiTextIntervalAndTimeIntervals)
        }

    // Make sure there is a Seq[TimEx] for each sentence.
    sentenceBundles.map { sentenceBundle => timExMap.getOrElse(sentenceBundle.index, Seq.empty) }
  }

  def parseTime(timenorm: Option[TemporalNeuralParser], regexs: Option[List[Regex]], documentCreationTime: Option[String]): Unit =
      times = timenorm.map(parseTime(_, regexs.get, documentCreationTime))
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
case class TimeStep(startDateOpt: Option[LocalDateTime], endDateOpt: Option[LocalDateTime], duration: Long)
@SerialVersionUID(1L)
case class TimEx(span: TextInterval, intervals: Seq[TimeStep], text: String)
@SerialVersionUID(1L)
case class DCT(interval: TimExInterval, text: String)


@SerialVersionUID(100L)
class DctDocumentAttachmentBuilderFromText extends DocumentAttachmentBuilderFromText {

  def mkDocumentAttachment(serializedText: String): DctDocumentAttachment = {
    // See also the version in JLDDeserializer.
    val Array(text, start, end) = serializedText.split('\t')
    val startDateTime = LocalDateTime.parse(start)
    val endDateTime = LocalDateTime.parse(end)
    val interval = SimpleInterval(startDateTime, endDateTime)
    val dct = DCT(interval, text)

    new DctDocumentAttachment(dct)
  }
}

@SerialVersionUID(100L)
class DctDocumentAttachmentBuilderFromJson extends DocumentAttachmentBuilderFromJson {

  def mkDocumentAttachment(dctValue: JValue): DctDocumentAttachment = {
    implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

    val text = (dctValue \ "text").extract[String]
    val startOpt = (dctValue \ "start").extractOpt[String]
    val endOpt = (dctValue \ "end").extractOpt[String]
    val dct =
        if (startOpt.isEmpty && endOpt.isEmpty) DCT(UnknownInterval, text)
        else {
          val start = startOpt.getOrElse(endOpt.get)
          val end = endOpt.getOrElse(startOpt.get)
          val startDateTime = LocalDateTime.parse(start)
          val endDateTime = LocalDateTime.parse(end)
          val interval = SimpleInterval(startDateTime, endDateTime)
          val dct = DCT(interval, text)

          dct
        }

    new DctDocumentAttachment(dct)
  }
}

class DctDocumentAttachment(val dct: DCT) extends DocumentAttachment { // Maybe with EidosAttachment for jsonld?

  override def documentAttachmentBuilderFromTextClassName: String = classOf[DctDocumentAttachmentBuilderFromText].getName
  override def documentAttachmentBuilderFromJsonClassName: String = classOf[DctDocumentAttachmentBuilderFromJson].getName

  override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[DctDocumentAttachment]

    this.dct == that.dct
  }

  override def toDocumentSerializer: String = {
    val start = dct.interval.start
    val end = dct.interval.end

    dct.text + "\t" + start.toString + "\t" + end.toString
  }

  override def toJsonSerializer: JValue = {
    val start = dct.interval.start
    val end = dct.interval.end

    ("text" -> dct.text) ~
        ("start" -> start.toString) ~
        ("end" -> end.toString)
  }
}

@SerialVersionUID(1L)
case class SentenceIndexAndTextInterval(sentenceIndex: Int, textInterval: TextInterval)
@SerialVersionUID(1L)
case class TextIntervalAndTimeIntervals(textInterval: TextInterval, timeIntervals: Seq[TimeInterval])
@SerialVersionUID(1L)
case class SentenceIndexAndTextIntervalAndTimeIntervals(sentenceIndex: Int, multiTextIntervalAndTimeIntervals: Seq[TextIntervalAndTimeIntervals])

@SerialVersionUID(1L)
case class SentenceBundle(index: Int, start: Int, sentence: Sentence, text: String, multiSentenceIndexAndTextInterval: Seq[SentenceIndexAndTextInterval], contexts: Seq[String])
