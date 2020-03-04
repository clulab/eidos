package org.clulab.wm.eidos.export

import java.io.{File, PrintWriter}

import ai.lum.common.StringUtils._
import org.clulab.odin.Attachment
import org.clulab.odin.{EventMention, Mention, State}
import org.clulab.serialization.json.stringify
import org.clulab.utils.Serializer
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.CountAttachment
import org.clulab.wm.eidos.attachments.Location
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.groundings.EidosOntologyGrounder
import org.clulab.wm.eidos.mentions.{EidosEventMention, EidosMention}
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.serialization.json.JLDRelationMigration
import org.clulab.wm.eidos.utils.{ExportUtils, FileUtils}
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.CsvWriter
import org.clulab.wm.eidos.utils.GroundingUtils.{getBaseGroundingString, getGroundingsString}
import org.clulab.wm.eidos.utils.TsvWriter


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

case class GroundingExporter(filename: String, reader: EidosSystem, groundAs: Seq[String], topN: Int = 5) extends Exporter {

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
        "Comments"
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

      direction = ExportUtils.poorMansIndra(cause, effect)
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
      "" // comments
    )
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

case class MigrationExporter(filename: String) extends Exporter {

  override def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    new TsvWriter(FileUtils.printWriterFromFile(filename)).autoClose { tsvWriter =>
      tsvWriter.println(
        "DocID",
        "Sentence Index",
        "Group Text",

        "Group Count Text",
        "Group Count Value",
        "Group Count Unit",

        "Group Modifier Text",
        "MoveTo Text",
        "MoveTo Location",
        "MoveFrom Text",
        "MoveFrom Location",
        "MoveThrough Text",
        "MoveThrough Location",
        "Sentence Text"
      )
      annotatedDocuments.foreach(printTableRows(_, tsvWriter))
    }
  }

  def printTableRows(annotatedDocument: AnnotatedDocument, tsvWriter: TsvWriter): Unit = {

    def getEidosArgumentOpt(eidosMention: EidosMention, name: String): (Option[EidosMention], String) = {
      val argumentsOpt = eidosMention.eidosArguments.get(name)
      val argumentOpt = argumentsOpt.flatMap(_.headOption)
      val text = argumentOpt.map(_.odinMention.text).getOrElse("")

      (argumentOpt, text)
    }

    def getLocation(eidosMentionOpt: Option[EidosMention]): String = {
      eidosMentionOpt.flatMap { eidosMention: EidosMention =>
        eidosMention.odinMention.attachments.collectFirst { case locationAttachment: Location =>
          locationAttachment.geoPhraseID.geonameID.getOrElse("")
        }
      }.getOrElse("")
    }

    annotatedDocument.eidosMentions.foreach { eidosMention: EidosMention =>
      if (eidosMention.odinMention matches "HumanMigration") {
        val        (groupOpt,         groupText) = getEidosArgumentOpt(eidosMention, "group")
        val               (_, groupModifierText) = getEidosArgumentOpt(eidosMention, "groupModifier")
        val        (moveToOpt,       moveToText) = getEidosArgumentOpt(eidosMention, "moveTo")
        val      (moveFromOpt,     moveFromText) = getEidosArgumentOpt(eidosMention, "moveFrom")
        val   (moveThroughOpt,  moveThroughText) = getEidosArgumentOpt(eidosMention, "moveThrough")

        val (groupCountText, groupCountValue, groupCountUnit) = groupOpt.flatMap { group =>
          group.odinMention.attachments.collectFirst { case countAttachment: CountAttachment =>
            (countAttachment.text, countAttachment.migrationGroupCount.value.toString, countAttachment.migrationGroupCount.unit.toString)
          }
        }.getOrElse(("", "", ""))

        tsvWriter.println(
          annotatedDocument.document.id.getOrElse(""),
          eidosMention.odinMention.sentence.toString,
          groupText,

          groupCountText,
          groupCountValue,
          groupCountUnit,

          groupModifierText,
          moveToText,
          getLocation(moveToOpt),
          moveFromText,
          getLocation(moveFromOpt),
          moveThroughText,
          getLocation(moveThroughOpt),
          eidosMention.odinMention.sentenceObj.getSentenceText
        )
      }
    }
  }
}
