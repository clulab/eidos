package org.clulab.wm.eidos.exporters

import ai.lum.common.StringUtils.StringWrapper
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.utils.{CsvWriter, FileUtils, MentionUtils}
import org.clulab.wm.eidos.utils.Closer.AutoCloser

case class GroundingAnnotationExporter(filename: String, reader: EidosSystem, groundAs: Seq[String], topN: Int = 5) extends Exporter {

  override def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    new CsvWriter(FileUtils.printWriterFromFile(filename)).autoClose { csvWriter =>
      csvWriter.println(
        "DocID",
        "Sentence ID",
        "Cause Text",
        "Cause Canonical Name",
        "TopN Groundings",
        "Cause Score",
        "Direction",
        "Effect Text",
        "Effect Canonical Name",
        "TopN Groundings",
        "Effect Score",
        "Relation Score",
        "Annotator",
        "Evidence",
        "Comments",
        "Trigger",
        "Rule",
        "Negated",
      )
      annotatedDocuments.foreach(printTableRows(_, csvWriter, reader))
      csvWriter.println()
    }
  }

  def printTableRows(annotatedDocument: AnnotatedDocument, csvWriter: CsvWriter, reader: EidosSystem): Unit = {
    val causalMentions = annotatedDocument.eidosMentions.filter(m => m.label == EidosSystem.CAUSAL_LABEL)

    for {
      mention <- causalMentions

      sentenceId = mention.odinMention.sentence
      docID = mention.odinMention.document.id.getOrElse("NONE")

      // For now, only put EidosEventMentions in the eval
      cause <- mention.eidosArguments("cause")
      causeInfo = EntityInfo(cause, groundAs, topN, delim = "\n")
      causeGroundings = causeInfo.groundingStrings.head // topN in a row, newline separatec

      effect <- mention.eidosArguments("effect")
      effectInfo = EntityInfo(effect, groundAs, topN, delim = "\n")
      effectGroundings = effectInfo.groundingStrings.head // topN in a row, newline separated

      trigger = MentionUtils.triggerOpt(mention).getOrElse("")
      direction = Exporter.poorMansIndra(cause, effect)
      negation = if (MentionUtils.hasNegation(mention)) "TRUE" else "false"
      evidence = mention.odinMention.sentenceObj.getSentenceText.normalizeSpace
    } csvWriter.println(
      docID,
      sentenceId.toString,
      causeInfo.text.normalizeSpace,
      causeInfo.canonicalName.normalizeSpace,
      causeGroundings,
      "", // cause grounding score
      direction,
      effectInfo.text.normalizeSpace,
      effectInfo.canonicalName.normalizeSpace,
      effectGroundings,
      "", // effect grounding score
      "", // relation score
      "", // annotator
      evidence,
      "", // comments
      trigger,
      mention.odinMention.foundBy,
      negation,
    )
  }
}
