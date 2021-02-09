package org.clulab.wm.eidos.serialization.simple

import org.clulab.wm.eidos.attachments.{Decrease, Increase}
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.Logging
import org.clulab.wm.eidoscommon.utils.TsvWriter

import java.io.BufferedWriter
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.nio.charset.StandardCharsets

class SimpleSerializer(annotatedDocument: AnnotatedDocument) {
  val allEidosMentions: Seq[EidosMention] = annotatedDocument.allEidosMentions

  def serialize(): Unit = {
    val printWriter = new PrintWriter(System.out, true)
    val tsvWriter = new TsvWriter(printWriter)

    serialize(tsvWriter)
    printWriter.println()
  }

  def serialize(filename: String): Unit = {
    val file = new File(filename)
    val charset = StandardCharsets.UTF_8.toString
    val printWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), charset)))
    val tsvWriter = new TsvWriter(printWriter)

    tsvWriter.autoClose { tsvWriter =>
      serialize(tsvWriter)
    }
  }

  // Find the index based on identity rather than equality.
  def indexOf(eidosMention: EidosMention): Int = allEidosMentions.indexWhere(_.eq(eidosMention))

  def getArgumentOpt(eidosMention: EidosMention, argumentName: String): Option[EidosMention] = {
    val argumentsOpt = eidosMention.eidosArguments.get(argumentName)

    if (argumentsOpt.isDefined && argumentsOpt.get.length > 1) {
      SimpleSerializer.logger.warn(s"""I can't deal with more than one $argumentName at a time in "${eidosMention.odinMention.text}".""")
      None
    }
    else
      argumentsOpt.map(_.head)
  }

  def getEffectOpt(eidosMention: EidosMention): Option[EidosMention] = getArgumentOpt(eidosMention, "effect")

  def getCauseOpt(eidosMention: EidosMention): Option[EidosMention] = getArgumentOpt(eidosMention, "cause")

  def isSplitSentence(eidosMention: EidosMention, causeMentionOpt: Option[EidosMention], effectMentionOpt: Option[EidosMention]): Boolean = {
    causeMentionOpt.isDefined && effectMentionOpt.isDefined && {
      val mentionSentence = eidosMention.odinMention.sentence
      val causeSentence = causeMentionOpt.get.odinMention.sentence
      val effectSentence = effectMentionOpt.get.odinMention.sentence

      mentionSentence != causeSentence || mentionSentence != effectSentence
    }
  }

  def getTextStartEndPolarity(eidosMentionOpt: Option[EidosMention]): (String, String, String, String) = {
    val polarity = getPolarity(eidosMentionOpt)
    eidosMentionOpt.map { eidosMention =>
      val text = eidosMention.odinMention.text
      val start = eidosMention.odinMention.start.toString
      val end = eidosMention.odinMention.end.toString
      (text, start, end, polarity)
    }.getOrElse("", "", "", polarity)
  }

  // This is highly subjective, so let someone else interpret it.
  def getPolarity(eidosMentionOpt: Option[EidosMention]): String = {
    eidosMentionOpt.map { eidosMention =>
      val attachments = eidosMention.odinMention.attachments
      val incCount = attachments.count{ attachment => attachment.isInstanceOf[Increase] }
      val decCount = attachments.count{ attachment => attachment.isInstanceOf[Decrease] }
      s"$incCount/$decCount"
    }.getOrElse("")
  }

  def serialize(tsvWriter: TsvWriter): Unit = {
    tsvWriter.println("Index",
      "Cause text", "Cause token start", "Cause token end", "Cause Inc/Dec",
      "Effect text", "Effect token start", "Effect token end", "Effect Inc/Dec",
      "Tokenized sentence")
    allEidosMentions
      .view
      .map { eidosMention =>
        val causeEidosMentionOpt = getCauseOpt(eidosMention)
        val effectEidosMentionOpt = getEffectOpt(eidosMention)
        (eidosMention, causeEidosMentionOpt, effectEidosMentionOpt)
      }
      .filter { case (_, causeEidosMentionOpt, effectEidosMentionOpt) =>
        // Candidates for printing
        causeEidosMentionOpt.isDefined || effectEidosMentionOpt.isDefined
      }
      .filter { case (eidosMention, causeEidosMentionOpt, effectEidosMentionOpt) =>
        // Those that won't fit into the schema
        if (isSplitSentence(eidosMention, causeEidosMentionOpt, effectEidosMentionOpt)) {
          SimpleSerializer.logger.warn(s"""I can't deal with a split sentence in "${eidosMention.odinMention.text}".""")
          false
        }
        else true
      }
      .zipWithIndex
      .foreach { case ((eidosMention, causeEidosMentionOpt, effectEidosMentionOpt), idx) =>
        val index = idx.toString
        val (causeText, causeTokenStart, causeTokenEnd, causePolarity) = getTextStartEndPolarity(causeEidosMentionOpt)
        val (effectText, effectTokenStart, effectTokenEnd, effectPolarity) = getTextStartEndPolarity(effectEidosMentionOpt)
        val tokenizedSentence = eidosMention.odinMention.sentenceObj.words.mkString(" ")

        tsvWriter.println(index,
          causeText, causeTokenStart, causeTokenEnd, causePolarity,
          effectText, effectTokenStart, effectTokenEnd, effectPolarity,
          tokenizedSentence
        )
      }
  }
}

object SimpleSerializer extends Logging {

  def apply(annotatedDocument: AnnotatedDocument): SimpleSerializer = new SimpleSerializer(annotatedDocument)
}
