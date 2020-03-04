package org.clulab.wm.eidos.export

import java.io.{File, PrintWriter}

import ai.lum.common.StringUtils._
import org.clulab.odin.{EventMention, Mention, State}
import org.clulab.serialization.json.stringify
import org.clulab.utils.Serializer
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.groundings.EidosOntologyGrounder
import org.clulab.wm.eidos.mentions.{EidosEventMention, EidosMention}
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.{ExportUtils, FileUtils}
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.GroundingUtils.{getBaseGroundingString, getGroundingsString}


trait Exporter {
  def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit
}

// Helper classes for facilitating the different export formats
case class JSONLDExporter(filename: String, reader: EidosSystem) extends Exporter {

  override def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    FileUtils.printWriterFromFile(filename).autoClose { pw =>
      val corpus = new JLDCorpus(annotatedDocuments)
      val mentionsJSONLD = corpus.serialize()

      pw.println(stringify(mentionsJSONLD, pretty = true))
    }
  }
}

case class MitreExporter(outFilename: String, reader: EidosSystem, filename: String, groundAs: Seq[String], topN: Int) extends Exporter {

  override def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    FileUtils.printWriterFromFile(outFilename).autoClose { pw =>
      pw.println(header())
      annotatedDocuments.foreach(printTableRows(_, pw, filename, reader))
    }
  }

  def header(): String = {
    val factors = Seq("A", "B").flatMap { factor =>
      groundAs.map { namespace =>
        s"Factor $factor top${topN}_${namespace.toUpperCase}Ontology"
      }
    }.mkString("\t")

    "Source\tSystem\tSentence ID\tFactor A Text\tFactor A Normalization\t" +
      "Factor A Modifiers\tFactor A Polarity\tRelation Text\tRelation Normalization\t" +
      "Relation Modifiers\tFactor B Text\tFactor B Normalization\tFactor B Modifiers\t" +
      "Factor B Polarity\tLocation\tTime\tEvidence\t" +
      factors
  }

  def printTableRows(annotatedDocument: AnnotatedDocument, pw: PrintWriter, filename: String, reader: EidosSystem): Unit = {
    val allOdinMentions = annotatedDocument.eidosMentions.map(_.odinMention)
    val mentionsToPrint = annotatedDocument.eidosMentions.filter(m => reader.components.stopwordManager.releventEdge(m.odinMention, State(allOdinMentions)))

    for {
      mention <- mentionsToPrint

      source = filename
      system = "Eidos"
      sentence_id = mention.odinMention.sentence

      // For now, only put EidosEventMentions in the mitre tsv
      if mention.isInstanceOf[EidosEventMention]
      cause <- mention.asInstanceOf[EidosEventMention].eidosArguments("cause")
      factor_a_info = EntityInfo(cause, groundAs)

      trigger = mention.odinMention.asInstanceOf[EventMention].trigger
      relation_txt = ExportUtils.removeTabAndNewline(trigger.text)
      relation_norm = mention.label // i.e., "Causal" or "Correlation"
      relation_modifier = ExportUtils.getModifier(mention) // prob none

      effect <- mention.asInstanceOf[EidosEventMention].eidosArguments("effect")
      factor_b_info = EntityInfo(effect, groundAs)

      location = "" // I could try here..?
      time = ""
      evidence = ExportUtils.removeTabAndNewline(mention.odinMention.sentenceObj.getSentenceText.trim)

      row = source + "\t" + system + "\t" + sentence_id + "\t" +
        factor_a_info.toTSV + "\t" +
        relation_txt + "\t" + relation_norm + "\t" + relation_modifier + "\t" +
        factor_b_info.toTSV + "\t" +
        location + "\t" + time + "\t" + evidence + "\t" + factor_a_info.groundingToTSV + "\t" +
        factor_b_info.groundingToTSV + "\n"
    } pw.print(row)
  }
}

case class GroundingExporter(pw: PrintWriter, reader: EidosSystem, groundAs: Seq[String], topN: Int = 5) extends Exporter {
  override def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    // Header
    pw.println(header)
    annotatedDocuments.foreach(printTableRows(_, pw, reader))
    pw.flush()
    pw.println()
    pw.close()
  }

  def header: String = {
    val headerRow = Seq(
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
      "Comments"
    )
    headerRow.mkString(",")
  }

  def printTableRows(annotatedDocument: AnnotatedDocument, pw: PrintWriter, reader: EidosSystem): Unit = {
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

      direction = ExportUtils.poorMansIndra(cause, effect)
      evidence = mention.odinMention.sentenceObj.getSentenceText.normalizeSpace

      row = Seq(
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
      )

      escaped = row.map(_.escapeCsv)

    } pw.println(escaped.mkString(","))
  }

}


case class SerializedExporter(filename: String) extends Exporter {

  override def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    val odinMentions = annotatedDocuments.flatMap(ad => ad.odinMentions)
    Serializer.save[SerializedMentions](new SerializedMentions(odinMentions), filename + ".serialized")
  }
}

// Helper Class to facilitate serializing the mentions
@SerialVersionUID(1L)
class SerializedMentions(val mentions: Seq[Mention]) extends Serializable {}
object SerializedMentions {
  def load(file: File): Seq[Mention] = Serializer.load[SerializedMentions](file).mentions
  def load(filename: String): Seq[Mention] = Serializer.load[SerializedMentions](filename).mentions
}

case class EntityInfo(
    m: EidosMention,
    groundAs: Seq[String] = Seq(EidosOntologyGrounder.PRIMARY_NAMESPACE),
    topN: Int = 5,
    delim: String = ", ") {
  val text: String = m.odinMention.text
  val canonicalName: String = m.canonicalName
  val norm: String = getBaseGroundingString(m)
  val modifier: String = ExportUtils.getModifier(m)
  val polarity: String = ExportUtils.getPolarity(m)
  val groundingStrings: Seq[String] = groundAs.map { namespace =>
    getGroundingsString(m, namespace, topN, delim)
  }


  def toTSV: String = Seq(text, norm, modifier, polarity).map(_.normalizeSpace).mkString("\t")

  def groundingToTSV: String = groundingStrings.map(_.normalizeSpace).mkString("\t")
}


