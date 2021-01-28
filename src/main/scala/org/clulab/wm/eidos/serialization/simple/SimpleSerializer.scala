package org.clulab.wm.eidos.serialization.simple

import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
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

    if (argumentsOpt.isDefined && argumentsOpt.get.length > 1)
      throw new RuntimeException(s"I can't deal with more than one $argumentName at a time.")
    argumentsOpt.map(_.head)
  }

  def getEffectOpt(eidosMention: EidosMention): Option[EidosMention] = getArgumentOpt(eidosMention, "effect")

  def getCauseOpt(eidosMention: EidosMention): Option[EidosMention] = getArgumentOpt(eidosMention, "cause")

  def serialize(tsvWriter: TsvWriter): Unit = {
    tsvWriter.println("Index", "Type", "Cause", "Effect", "Sentence", "Start", "End", "Text")
    allEidosMentions.zipWithIndex.foreach { case (eidosMention, idx) =>
      val index = idx.toString
      val typ = eidosMention.getClass.getSimpleName
      val cause = getCauseOpt(eidosMention).map(indexOf(_).toString).getOrElse("")
      val effect = getEffectOpt(eidosMention).map(indexOf(_).toString).getOrElse("")
      val sentence = eidosMention.odinMention.sentence.toString
      val start = eidosMention.odinMention.startOffset.toString
      val end = eidosMention.odinMention.endOffset.toString
      val text = eidosMention.odinMention.text

      tsvWriter.println(index, typ, cause, effect, sentence, start, end, text)
    }
  }
}

object SimpleSerializer {

  def apply(annotatedDocument: AnnotatedDocument): SimpleSerializer = new SimpleSerializer(annotatedDocument)
}
