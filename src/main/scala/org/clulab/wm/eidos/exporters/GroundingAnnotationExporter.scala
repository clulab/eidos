package org.clulab.wm.eidos.exporters

import ai.lum.common.StringUtils.StringWrapper
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.{CsvWriter, FileUtils, MentionUtils}
import org.clulab.wm.eidos.utils.Closer.AutoCloser

import scala.collection.Seq

class GroundingAnnotationExporter(filename: String, reader: EidosSystem, groundAs: Seq[String], topN: Int = 5) extends Exporter {

  override def export(annotatedDocument: AnnotatedDocument): Unit = {
    new CsvWriter(FileUtils.printWriterFromFile(filename)).autoClose { csvWriter =>
      printHeader(csvWriter)
      printRows(annotatedDocument, csvWriter, reader)
      csvWriter.println()
    }
  }

  def printHeader(csvWriter: CsvWriter): Unit = {
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
      "Negated"
    )
  }

  def printRows(annotatedDocument: AnnotatedDocument, csvWriter: CsvWriter, reader: EidosSystem): Unit = {
    val causalMentions = annotatedDocument.eidosMentions.filter(m => m.label == EidosSystem.CAUSAL_LABEL)
    writeMentions(causalMentions, csvWriter)
  }

  def writeMentions(mentions: Seq[EidosMention], csvWriter: CsvWriter): Unit = {
    val rows = getRows(mentions)
    rows.foreach(csvWriter.print)
  }

  def getRows(mentions: Seq[EidosMention]): Seq[Seq[String]] = {
    for {
      mention <- mentions
      sentenceId = mention.odinMention.sentence
      docID = mention.odinMention.document.id.getOrElse("NONE")

      // For now, only put EidosEventMentions in the eval
      cause <- mention.eidosArguments.getOrElse("cause", Seq())
      causeInfo = EntityInfo(cause, groundAs, topN, delim = "\n")
      causeGroundings = causeInfo.groundingStrings.head // topN in a row, newline separatec


      effect <- mention.eidosArguments.getOrElse("effect", Seq())
      effectInfo = EntityInfo(effect, groundAs, topN, delim = "\n")
      effectGroundings = effectInfo.groundingStrings.head // topN in a row, newline separated


      trigger = MentionUtils.triggerOpt(mention).getOrElse("")
      direction = Exporter.poorMansIndra(cause, effect)
      negation = if (MentionUtils.hasNegation(mention)) "TRUE" else "false"
      evidence = mention.odinMention.sentenceObj.getSentenceText.normalizeSpace

      // Intervention specific eval
      //      topCauseGrounding = EntityInfo(cause, groundAs, 1, delim = "\n").groundingStrings.head
      //      topCauseScore = GroundingUtils.getGroundingOpt(cause, "wm_flat").flatMap(_.headOption).map(_._2).getOrElse(0.0f)
      //      topEffectGrounding = EntityInfo(effect, groundAs, 1, delim = "\n").groundingStrings.head
      //      topEffectScore = GroundingUtils.getGroundingOpt(effect, "wm_flat").flatMap(_.headOption).map(_._2).getOrElse(0.0f)
      //      if topCauseGrounding.contains("causal_factor/interventions") || topEffectGrounding.contains("causal_factor/interventions")

    } yield Seq(
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
      negation
    )
  }
}
