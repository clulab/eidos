package org.clulab.wm.eidos.export

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.{Decrease, Increase}
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.{FileUtils, GroundingUtils}


case class UnmodifiedTSVExporter(outFilename: String, filename: String) extends Exporter {

  override def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    FileUtils.printWriterFromFile(outFilename).autoClose { pw =>
      for (ad <- annotatedDocuments) {
        val ms = ad.eidosMentions.filter(_.label == EidosSystem.CAUSAL_LABEL)
        for (m <- ms) {
          val unmodCause = unmodifiedArg(m, "cause")
          val unmodEffect = unmodifiedArg(m, "effect")
          val unmodBoth = unmodCause && unmodEffect
          if (unmodCause || unmodEffect) {
            val tsvContent = Seq(filename, m.odinMention.sentence, "",
              unmodCause, "",
              m.eidosArguments("cause").head.odinMention.text, m.eidosArguments("cause").head.canonicalName,
              //            GroundingUtils.getGroundingsString(m.eidosArguments("cause").head, "wm", 1),
              unmodEffect, "",
              m.eidosArguments("effect").head.odinMention.text, m.eidosArguments("effect").head.canonicalName,
              //            GroundingUtils.getGroundingsString(m.eidosArguments("effect").head, "wm", 1),
              unmodBoth, ""
            )
            pw.println(tsvContent.mkString("\t"))
          }
        }
      }
    }
  }

  def unmodifiedArg(m: EidosMention, argName: String): Boolean = {
    // Only looking at causal relations
    assert(m.label == EidosSystem.CAUSAL_LABEL)
    val argument = m.odinArguments(argName).head
    val incDecAttachments = argument.attachments.collect{
      case a: Increase => a
      case a: Decrease => a
    }
    incDecAttachments.isEmpty
  }
}
