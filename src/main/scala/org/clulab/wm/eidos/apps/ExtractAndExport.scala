package org.clulab.wm.eidos.apps

import java.io.PrintWriter

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.utils.Serializer
import org.clulab.odin.{EventMention, Mention}
import org.clulab.serialization.json.stringify
import org.clulab.utils.Configured
import org.clulab.wm.eidos.attachments.{Decrease, Increase, Quantification}
import org.clulab.wm.eidos.groundings.EidosOntologyGrounder
import org.clulab.wm.eidos.mentions.{EidosEventMention, EidosMention}
import org.clulab.wm.eidos.{AnnotatedDocument, EidosSystem}
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.FileUtils.{findFiles, printWriterFromFile}
import org.clulab.wm.eidos.utils.GroundingUtils.{getBaseGrounding, getGroundingsString}

import scala.collection.mutable.ArrayBuffer




/**
  * App used to extract mentions from files in a directory and produce the desired output format (i.e., jsonld, mitre
  * tsv or serialized mentions).  The input and output directories as well as the desired export formats are specified
  * in eidos.conf (located in src/main/resources).
  */
object ExtractAndExport extends App with Configured {

  def getExporter(exporterString: String, filename: String, topN: Int): Exporter = {
    exporterString match {
      case "jsonld" => JSONLDExporter(printWriterFromFile(filename + ".jsonld"), reader)
      case "mitre" => MitreExporter(printWriterFromFile(filename + ".mitre.tsv"), reader, filename, topN)
      case "serialized" => SerializedExporter(filename)
      case _ => throw new NotImplementedError(s"Export mode $exporterString is not supported.")
    }
  }

  val config = ConfigFactory.load("eidos")
  override def getConf: Config = config

  val inputDir = getArgString("apps.inputDirectory", None)
  val outputDir = getArgString("apps.outputDirectory", None)
  val inputExtension = getArgString("apps.inputFileExtension", None)
  val exportAs = getArgStrings("apps.exportAs", None)
  val topN = getArgInt("apps.groundTopN", Some(5))

  val files = findFiles(inputDir, inputExtension)
  val reader = new EidosSystem()

  // For each file in the input directory:
  files.par.foreach { file =>
    // 1. Open corresponding output file and make all desired exporters
    println(s"Extracting from ${file.getName}")
    val exporters = exportAs.map(getExporter(_, s"$outputDir/${file.getName}", topN))
    // 2. Get the input file contents
    val text = FileUtils.getTextFromFile(file)
    // 3. Extract causal mentions from the text
    val annotatedDocuments = Seq(reader.extractFromText(text))
    // 4. Export to all desired formats
    exporters.foreach(_.export(annotatedDocuments))
  }

}


trait Exporter {
  def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit
}

// Helper classes for facilitating the different export formats
case class JSONLDExporter (pw: PrintWriter, reader: EidosSystem) extends Exporter {
  override def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    val corpus = new JLDCorpus(annotatedDocuments, reader)
    val mentionsJSONLD = corpus.serialize()
    pw.println(stringify(mentionsJSONLD, pretty = true))
    pw.close()
  }
}

case class MitreExporter (pw: PrintWriter, reader: EidosSystem, filename: String, topN: Int) extends Exporter {
  override def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    // Header
    pw.println(header())
    annotatedDocuments.foreach(printTableRows(_, pw, filename, reader))
    pw.close()
  }

  def header(): String = "Source\tSystem\tSentence ID\tFactor A Text\tFactor A Normalization\t" +
    "Factor A Modifiers\tFactor A Polarity\tRelation Text\tRelation Normalization\t" +
    "Relation Modifiers\tFactor B Text\tFactor B Normalization\tFactor B Modifiers\t" +
    "Factor B Polarity\tLocation\tTime\tEvidence\t" +
    s"Factor A top${topN}_UNOntology\tFactor A top${topN}_FAOOntology\tFactor A top${topN}_WDIOntology" +
    s"Factor B top${topN}_UNOntology\tFactor B top${topN}_FAOOntology\tFactor B top${topN}_WDIOntology"

  def getModifier(mention: EidosMention): String = {
    val attachments = mention.odinMention.attachments
    val quantTriggers = attachments
      .filter(a => a.isInstanceOf[Quantification])
      .map(quant => quant.asInstanceOf[Quantification].trigger)
      .map(t => t.toLowerCase)

    if (quantTriggers.nonEmpty) s"${quantTriggers.mkString(", ")}" else ""
  }

  //fixme: not working -- always ;
  def getPolarity(mention: EidosMention): String = {
    val sb = new ArrayBuffer[String]
    val attachments = mention.odinMention.attachments
    val incTriggers = attachments.filter(a => a.isInstanceOf[Increase]).map(inc => inc.asInstanceOf[Increase].trigger)
    val decTriggers = attachments.filter(a => a.isInstanceOf[Decrease]).map(inc => inc.asInstanceOf[Decrease].trigger)
    for (t <- incTriggers) sb.append(s"Increase(${t})")
    for (t <- decTriggers) sb.append(s"Decrease(${t})")

    sb.mkString(", ")
  }

  def printTableRows(annotatedDocument: AnnotatedDocument, pw: PrintWriter, filename: String, reader: EidosSystem): Unit = {
    val mentionsToPrint = annotatedDocument.eidosMentions.filter(m => reader.releventEdge(m.odinMention))

    for {
      mention <- mentionsToPrint

      source = filename
      system = "Eidos"
      sentence_id = mention.odinMention.sentence

      cause <- mention.asInstanceOf[EidosEventMention].eidosArguments("cause")

      factor_a_txt = removeTabAndNewline(cause.odinMention.text)
      factor_a_norm = getBaseGrounding(cause)
      factor_a_modifier = getModifier(cause)
      factor_a_polarity = getPolarity(cause)
      factor_a_un = getGroundingsString(cause, EidosOntologyGrounder.UN_NAMESPACE, topN)
      factor_a_fao = getGroundingsString(cause, EidosOntologyGrounder.FAO_NAMESPACE, topN)
      factor_a_wdi = getGroundingsString(cause, EidosOntologyGrounder.WDI_NAMESPACE, topN)

      trigger = mention.odinMention.asInstanceOf[EventMention].trigger
      relation_txt = removeTabAndNewline(trigger.text)
      relation_norm = mention.label // i.e., "Causal" or "Correlation"
      relation_modifier = getModifier(mention) // prob none

      effect <- mention.asInstanceOf[EidosEventMention].eidosArguments("effect")
      factor_b_txt = removeTabAndNewline(effect.odinMention.text)
      factor_b_norm = getBaseGrounding(effect)
      factor_b_modifier = getModifier(effect)
      factor_b_polarity = getPolarity(effect)
      factor_b_un = getGroundingsString(effect, EidosOntologyGrounder.UN_NAMESPACE, topN)
      factor_b_fao = getGroundingsString(effect, EidosOntologyGrounder.FAO_NAMESPACE, topN)
      factor_b_wdi = getGroundingsString(effect, EidosOntologyGrounder.WDI_NAMESPACE, topN)

      location = "" // I could try here..?
      time = ""
      evidence = removeTabAndNewline(mention.odinMention.sentenceObj.getSentenceText().trim)

      row = source + "\t" + system + "\t" + sentence_id + "\t" +
        factor_a_txt + "\t" + factor_a_norm + "\t" + factor_a_modifier + "\t" + factor_a_polarity + "\t" +
        relation_txt + "\t" + relation_norm + "\t" + relation_modifier + "\t" +
        factor_b_txt + "\t" + factor_b_norm + "\t" + factor_b_modifier + "\t" + factor_b_polarity + "\t" +
        location + "\t" + time + "\t" + evidence + "\t" +
        factor_a_un + "\t" + factor_a_fao + "\t" + factor_a_wdi + "\t" +
        factor_b_un + "\t" + factor_b_fao + "\t" + factor_b_wdi + "\n"

    } pw.print(row)
  }

  def removeTabAndNewline(s: String) = s.replaceAll("(\\n|\\t)", " ")

}


case class SerializedExporter (filename: String) extends Exporter {
  override def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    val odinMentions = annotatedDocuments.flatMap(ad => ad.odinMentions)
    Serializer.save[SerializedMentions](new SerializedMentions(odinMentions), filename + ".serialized")
  }
}


// Helper Class to facilitate serializing the mentions
class SerializedMentions(val mentions: Seq[Mention]) extends Serializable {}
object SerializedMentions {
  def load(filename: String): Seq[Mention] = Serializer.load[SerializedMentions](filename).mentions
}
