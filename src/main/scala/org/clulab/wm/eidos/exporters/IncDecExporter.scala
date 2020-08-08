package org.clulab.wm.eidos.exporters

import java.io.PrintWriter

import org.clulab.wm.eidos.attachments.Decrease
import org.clulab.wm.eidos.attachments.Increase
import org.clulab.wm.eidos.attachments.TriggeredAttachment
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.TsvWriter

case class IncDecExporter(printWriter: PrintWriter) extends Exporter {
  val tsvWriter = new TsvWriter(printWriter)

  tsvWriter.println("words", "mention", "index", "inc/dec", "attachment", "start", "end")

  override def export(annotatedDocument: AnnotatedDocument): Unit = {
    annotatedDocument.allEidosMentions.foreach { eidosMention: EidosMention =>
      val attachments = eidosMention.odinMention.attachments
          .filter { attachment =>
            attachment.isInstanceOf[Increase] || attachment.isInstanceOf[Decrease]
          }
          .map(_.asInstanceOf[TriggeredAttachment])

      if (attachments.size > 1) {
        attachments.zipWithIndex.foreach { case (attachment, index) =>
          val startOpt = attachment.triggerProvenance.map(_.interval.start)
          val endOpt = attachment.triggerProvenance.map(_.interval.end)

          tsvWriter.println(
            eidosMention.odinMention.sentenceObj.words.mkString(" "),
            eidosMention.odinMention.text,
            index.toString,
            if (attachment.isInstanceOf[Increase]) "inc" else "dec",
            attachment.trigger,
            startOpt.map(_.toString).getOrElse(""),
            endOpt.map(_.toString).getOrElse("")
          )
        }
      }
    }
  }
}

