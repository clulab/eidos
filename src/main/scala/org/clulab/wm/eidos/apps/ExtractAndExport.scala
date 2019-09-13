package org.clulab.wm.eidos.apps

import java.io.{File, PrintWriter}

import ai.lum.common.StringUtils._
import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.utils.Serializer
import org.clulab.odin.{Attachment, EventMention, Mention, State}
import org.clulab.serialization.json.stringify
import org.clulab.utils.Configured
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.mentions.{EidosEventMention, EidosMention}
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.{ExportUtils, FileUtils}
import org.clulab.wm.eidos.utils.GroundingUtils.{getBaseGrounding, getGroundingsString}

import scala.collection.mutable.ArrayBuffer

/**
  * App used to extract mentions from files in a directory and produce the desired output format (i.e., jsonld, mitre
  * tsv or serialized mentions).  The input and output directories as well as the desired export formats are specified
  * in eidos.conf (located in src/main/resources).
  */
object ExtractAndExport extends App with Configured {



  val config = ConfigFactory.load("eidos")
  override def getConf: Config = config

  val inputDir = getArgString("apps.inputDirectory", None)
  val outputDir = getArgString("apps.outputDirectory", None)
  val inputExtension = getArgString("apps.inputFileExtension", None)
  val exportAs = getArgStrings("apps.exportAs", None)
  val groundAs = getArgStrings("apps.groundAs", None)
  val topN = getArgInt("apps.groundTopN", Some(5))
  val files = FileUtils.findFiles(inputDir, inputExtension)
  val reader = new EidosSystem()

  // For each file in the input directory:
  files.par.foreach { file =>
    // 1. Open corresponding output file and make all desired exporters
    println(s"Extracting from ${file.getName}")
    // 2. Get the input file contents
    val text = FileUtils.getTextFromFile(file)
    // 3. Extract causal mentions from the text
    val annotatedDocuments = Seq(reader.extractFromText(text, filename = Some(file.getName)))
    // 4. Export to all desired formats
    exportAs.foreach { format =>
      ExportUtils.getExporter(format, s"$outputDir/${file.getName}", reader, groundAs, topN).export(annotatedDocuments)
    }
  }
}

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

case class GroundingExporter(pw: PrintWriter, reader: EidosSystem, groundAs: Seq[String]) extends Exporter {
  override def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    // Header
    pw.println(header())
    annotatedDocuments.foreach(printTableRows(_, pw, reader))
  }

  def header(): String = {
    "Sentence ID\t" +
    "Factor A SCORE\t" +
    "Factor A Text\t" +
    "Factor A Rank 1\t" +
    "Factor A Rank 2\t" +
    "Factor A Rank 3\t" +
    "Factor A Rank 4\t" +
    "Factor A Rank 5\t" +
    "Factor B SCORE\t" +
    "Factor B Text\t" +
    "Factor B Rank 1\t" +
    "Factor B Rank 2\t" +
    "Factor B Rank 3\t" +
    "Factor B Rank 4\t" +
    "Factor B Rank 5\t" +
    "Evidence"
  }

  def printTableRows(annotatedDocument: AnnotatedDocument, pw: PrintWriter, reader: EidosSystem): Unit = {
    val allOdinMentions = annotatedDocument.eidosMentions.map(_.odinMention)
    val mentionsToPrint = annotatedDocument.eidosMentions.filter(m => reader.components.stopwordManager.releventEdge(m.odinMention, State(allOdinMentions)))

    for {
      mention <- mentionsToPrint

      sentence_id = mention.odinMention.sentence

      // For now, only put EidosEventMentions in the mitre tsv
      if mention.isInstanceOf[EidosEventMention]
      cause <- mention.asInstanceOf[EidosEventMention].eidosArguments("cause")
      factor_a_info = EntityInfo(cause, groundAs, topN = 5, delim = "\t")
      factor_a_groundings = factor_a_info.groundingStrings.head // five in a row, tab-separated

      effect <- mention.asInstanceOf[EidosEventMention].eidosArguments("effect")
      factor_b_info = EntityInfo(effect, groundAs, topN = 5, delim = "\t")
      factor_b_groundings = factor_b_info.groundingStrings.head // five in a row, tab-separated

      evidence = ExportUtils.removeTabAndNewline(mention.odinMention.sentenceObj.getSentenceText.trim)

      row = sentence_id + "\t" +
        "\t" + // score
        factor_a_info.text + "\t" +
        factor_a_groundings + "\t" +
        "\t" + // score
        factor_b_info.text + "\t" +
        factor_b_groundings + "\t" +
        evidence
    } pw.println(row)
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

case class EntityInfo(m: EidosMention, groundAs: Seq[String], topN: Int = 5, delim: String = ", ") {
  val text: String = m.odinMention.text
  val norm: String = getBaseGrounding(m)
  val modifier: String = ExportUtils.getModifier(m)
  val polarity: String = ExportUtils.getPolarity(m)
  val groundingStrings: Seq[String] = groundAs.map { namespace =>
    getGroundingsString(m, namespace, topN, delim)
  }


  def toTSV: String = Seq(text, norm, modifier, polarity).map(_.normalizeSpace).mkString("\t")

  def groundingToTSV: String = groundingStrings.map(_.normalizeSpace).mkString("\t")
}

