package org.clulab.wm.eidos.exporters

import org.clulab.odin.State
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.mentions.EidosEventMention
import org.clulab.wm.eidos.utils.{FileUtils, MentionUtils}
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.TsvWriter

@deprecated("MitreExporter will be deprecated", "0.3.0")
case class MitreExporter(outFilename: String, reader: EidosSystem, filename: String, groundAs: Seq[String], topN: Int) extends Exporter {

  protected def getHeaders: Seq[String] = {
    val staticHeaders = Seq(
      "Source",
      "System",
      "Sentence ID",

      "Factor A Text",
      "Factor A Normalization",
      "Factor A Modifiers",
      "Factor A Polarity",

      "Relation Text",
      "Relation Normalization",
      "Relation Modifiers",

      "Factor B Text",
      "Factor B Normalization",
      "Factor B Modifiers",
      "Factor B Polarity",

      "Location",
      "Time",
      "Evidence"
    )
    val dynamicHeaders = Seq("A", "B").flatMap { factor =>
      groundAs.map { namespace =>
        s"Factor $factor top${topN}_${namespace.toUpperCase}Ontology"
      }
    }

    staticHeaders ++ dynamicHeaders
  }

  protected def printTableRows(annotatedDocument: AnnotatedDocument, tsvWriter: TsvWriter, filename: String, reader: EidosSystem): Unit = {
    val allOdinMentions = annotatedDocument.eidosMentions.map(_.odinMention)
    val mentionsToPrint = annotatedDocument.eidosMentions.filter(m => reader.components.stopwordManager.releventEdge(m.odinMention, State(allOdinMentions)))

    for {
      mention <- mentionsToPrint

      // For now, only put EidosEventMentions in the mitre tsv
      if mention.isInstanceOf[EidosEventMention]

      cause <- mention.asInstanceOf[EidosEventMention].eidosArguments("cause")
      factor_a_info = EntityInfo(cause, groundAs)
      trigger = MentionUtils.triggerOpt(mention).getOrElse("")

      effect <- mention.asInstanceOf[EidosEventMention].eidosArguments("effect")
      factor_b_info = EntityInfo(effect, groundAs)
      evidence = Exporter.removeTabAndNewline(mention.odinMention.sentenceObj.getSentenceText.trim)
    } tsvWriter
        .print(
          filename,
          "Eidos",
          mention.odinMention.sentence.toString
        )
        .print(factor_a_info.toTSV)
        .print(
          Exporter.removeTabAndNewline(trigger), // Relation Text
          mention.label, // Relation Label, i.e., "Causal" or "Correlation",
          Exporter.getModifier(mention), // Relation Modifier, prob none
        )
        .print(factor_b_info.toTSV)
        .print(
          "", // Location
          "", // Time
          evidence
        )
        .print(factor_a_info.groundingToTSV)
        .print(factor_b_info.groundingToTSV)
  }

  override def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    new TsvWriter(FileUtils.printWriterFromFile(outFilename)).autoClose { tsvWriter =>
      tsvWriter.println(getHeaders)
      annotatedDocuments.foreach(printTableRows(_, tsvWriter, filename, reader))
    }
  }
}
