package org.clulab.wm.eidos.apps

import java.io.{File, PrintWriter}

import ai.lum.common.StringUtils._
import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.utils.Serializer
import org.clulab.odin._
import org.clulab.serialization.json.stringify
import org.clulab.utils.Configured
import org.clulab.wm.eidos.attachments._
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
      case "vanillajson" => VanillaOdinJsonExporter(new File(filename + ".json"))
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
    val annotatedDocuments = Seq(reader.extractFromText(text, filename = Some(file.getName)))
    // 4. Export to all desired formats
    exporters.foreach(_.export(annotatedDocuments))
  }

}


trait Exporter {
  def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit
}

// Helper classes for facilitating the different export formats
case class JSONLDExporter(pw: PrintWriter, reader: EidosSystem) extends Exporter {
  override def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    val corpus = new JLDCorpus(annotatedDocuments, reader)
    val mentionsJSONLD = corpus.serialize()
    pw.println(stringify(mentionsJSONLD, pretty = true))
    pw.close()
  }
}

case class MitreExporter(pw: PrintWriter, reader: EidosSystem, filename: String, topN: Int) extends Exporter {
  override def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    // Header
    pw.println(header())
    annotatedDocuments.foreach(printTableRows(_, pw, filename, reader))
    pw.close()
  }

  def header(): String = {
    "Source\tSystem\tSentence ID\tFactor A Text\tFactor A Normalization\t" +
      "Factor A Modifiers\tFactor A Polarity\tRelation Text\tRelation Normalization\t" +
      "Relation Modifiers\tFactor B Text\tFactor B Normalization\tFactor B Modifiers\t" +
      "Factor B Polarity\tLocation\tTime\tEvidence\t" +
      s"Factor A top${topN}_UNOntology\tFactor A top${topN}_FAOOntology\tFactor A top${topN}_WDIOntology" +
      s"Factor B top${topN}_UNOntology\tFactor B top${topN}_FAOOntology\tFactor B top${topN}_WDIOntology"
  }


  def printTableRows(annotatedDocument: AnnotatedDocument, pw: PrintWriter, filename: String, reader: EidosSystem): Unit = {
    val allOdinMentions = annotatedDocument.eidosMentions.map(_.odinMention)
    val mentionsToPrint = annotatedDocument.eidosMentions.filter(m => reader.releventEdge(m.odinMention, State(allOdinMentions)))

    for {
      mention <- mentionsToPrint

      source = filename
      system = "Eidos"
      sentence_id = mention.odinMention.sentence

      // For now, only put EidosEventMentions in the mitre tsv
      if mention.isInstanceOf[EidosEventMention]
      cause <- mention.asInstanceOf[EidosEventMention].eidosArguments("cause")
      factor_a_info = EntityInfo(cause)

      trigger = mention.odinMention.asInstanceOf[EventMention].trigger
      relation_txt = ExporterUtils.removeTabAndNewline(trigger.text)
      relation_norm = mention.label // i.e., "Causal" or "Correlation"
      relation_modifier = ExporterUtils.getModifier(mention) // prob none

      effect <- mention.asInstanceOf[EidosEventMention].eidosArguments("effect")
      factor_b_info = EntityInfo(effect)

      location = "" // I could try here..?
      time = ""
      evidence = ExporterUtils.removeTabAndNewline(mention.odinMention.sentenceObj.getSentenceText.trim)

      row = source + "\t" + system + "\t" + sentence_id + "\t" +
        factor_a_info.toTSV() + "\t" +
        relation_txt + "\t" + relation_norm + "\t" + relation_modifier + "\t" +
        factor_b_info.toTSV() + "\t" +
        location + "\t" + time + "\t" + evidence + "\t" + factor_a_info.groundingToTSV + "\t" +
        factor_b_info.groundingToTSV + "\n"


    } pw.print(row)
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


case class VanillaOdinJsonExporter(file: File) extends Exporter {
  import org.clulab.odin.serialization.json._
  import org.clulab.serialization.json._

  val CAUSE = "cause"
  val EFFECT = "effect"

  val NODE_LABEL = "Node" // org.clulab.influencer.ie.OdinUtils.NODE_LABEL

  // org.clulab.influencer.assembly.DeduplicationUtils
  val causeRole = "controller"
  val effectRole = "controlled"
  val causalRelation = "CausalEvent"
  val increaseEvent = "IncreaseEvent"
  val decreaseEvent = "DecreaseEvent"

  val INCREASE = "increases"
  val DECREASE = "decreases"



  override def export(annotatedDocuments: Seq[AnnotatedDocument]): Unit = {
    // convert eidos mentions of interest to odin mentions
    val vanillaMentions = for {
      event <- annotatedDocuments.flatMap(_.odinMentions).collect { case m: EventMention => m }
      vanilla <- convertToPromoteInhibit(event)
      //if Constraints.validEdge(hEdge)
    } yield vanilla

    // export the odin mentions
    vanillaMentions.saveJSON(file, pretty = false)
  }

  /// ------------------------------------------------------------------------------------------
  //                             Code brought in from lum.ai repo
  /// ------------------------------------------------------------------------------------------


  def convertToPromoteInhibit(m: Mention): Option[Mention] = {
    if (m matches EidosSystem.CAUSAL_LABEL) {
      val cause = m.arguments("cause").head
      val effect = m.arguments("effect").head
      val causePolarity = nodePolarity(cause)
      val effectPolarity = nodePolarity(effect)
      val eventPolarity = causePolarity * effectPolarity
      eventPolarity match {
        case 0 => None
        case 1 => Some(convertMention(m, INCREASE)) //todo
        case -1 => Some(convertMention(m, DECREASE)) //todo
        case _ => ??? // this should never happen because nodePolarity only returns -1, 0, or 1
      }
    } else {
      // FIXME: we are ignoring correlation and coreference
      // Some(m)
      None
    }
  }

  def convertMention(m: Mention, label: String): Mention = {
    // NOTE: we are ignoring time, geolocation, and quantifiers
    // check if mention is negated or hedged
    val negated = m.attachments.exists(_.isInstanceOf[Negation])
    val hedged = m.attachments.exists(_.isInstanceOf[Hedging])
    // generate new labels for mention
    val labels = label +: causalRelation +: m.labels
    val mentionId = m.id
    // return new mention with new labels and no attachments
    val mention = m match {
      case em: EventMention =>
        val causeOrig = m.arguments(CAUSE).head.asInstanceOf[TextBoundMention]
        val cause = causeOrig.copy(labels = NODE_LABEL +: causeOrig.labels)
        val effectOrig = m.arguments(EFFECT).head.asInstanceOf[TextBoundMention]
        val effect = effectOrig.copy(labels = NODE_LABEL +: effectOrig.labels)
        val arguments = Map(causeRole -> List(cause), effectRole -> List(effect))
        em.copy(labels = labels, attachments = Set.empty, arguments = arguments)
      case rm: RelationMention =>
        val causeOrig = m.arguments(CAUSE).head.asInstanceOf[TextBoundMention]
        val cause = causeOrig.copy(labels = NODE_LABEL +: causeOrig.labels)
        val effectOrig = m.arguments(EFFECT).head.asInstanceOf[TextBoundMention]
        val effect = effectOrig.copy(labels = NODE_LABEL +: effectOrig.labels)
        val arguments = Map(causeRole -> List(cause), effectRole -> List(effect))
        rm.copy(labels = labels, attachments = Set.empty, arguments = arguments)
      case tbm: TextBoundMention =>
        tbm.copy(labels = labels, attachments = Set.empty)
    }

    mention
  }

  def nodePolarity(m: Mention): Int = {
    val increases = m.attachments.filter(_.isInstanceOf[Increase]).toSeq
    val decreases = m.attachments.filter(_.isInstanceOf[Decrease]).toSeq
    (increases, decreases) match {
      case (Seq(), Seq()) => 1 // both empty
      case (inc, Seq()) => 1 // empty decreases
      case (Seq(), Seq(dec)) => -1 // one decrease and no increases
      case _ => 0 // TODO there may be other situations we can handle
    }
  }


}


case class EntityInfo(m: EidosMention, topN: Int = 5) {
  val text = m.odinMention.text
  val norm = getBaseGrounding(m)
  val modifier = ExporterUtils.getModifier(m)
  val polarity = ExporterUtils.getPolarity(m)
  val un = getGroundingsString(m, EidosOntologyGrounder.UN_NAMESPACE, topN)
  val fao = getGroundingsString(m, EidosOntologyGrounder.FAO_NAMESPACE, topN)
  val wdi = getGroundingsString(m, EidosOntologyGrounder.WDI_NAMESPACE, topN)

  def toTSV(): String = Seq(text, norm, modifier, polarity).map(_.normalizeSpace).mkString("\t")

  def groundingToTSV() = Seq(un, fao, wdi).map(_.normalizeSpace).mkString("\t")


}

object ExporterUtils {
  def getModifier(mention: EidosMention): String = {
    def quantHedgeString(a: Attachment): Option[String] = a match {
      case q: Quantification => Some(f"Quant(${q.trigger.toLowerCase})")
      case h: Hedging => Some(f"Hedged(${h.trigger.toLowerCase})")
      case n: Negation => Some(f"Negated(${n.trigger.toLowerCase})")
      case _ => None
    }

    val attachments = mention.odinMention.attachments.map(quantHedgeString).toSeq.filter(_.isDefined)

    val modifierString = attachments.map(a => a.get).mkString(", ")
    modifierString
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

  def removeTabAndNewline(s: String) = s.replaceAll("(\\n|\\t)", " ")
}
