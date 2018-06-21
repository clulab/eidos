package org.clulab.wm.eidos.apps

import java.io.PrintWriter

import org.clulab.odin.EventMention
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.{AnnotatedDocument, EidosSystem}
import org.clulab.wm.eidos.attachments._
import org.clulab.wm.eidos.groundings.EidosOntologyGrounder
import org.clulab.wm.eidos.mentions.{EidosEventMention, EidosMention}
import org.clulab.wm.eidos.utils.Sourcer
import org.clulab.wm.eidos.utils.GroundingUtils._

import scala.collection.mutable.ArrayBuffer
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.apps.ExtractFromDirectory.reader
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.FileUtils

object ExtractFromDirectory extends App {
  val inputDir = args(0)
  val outputDir = args(1)
  val files = findFiles(inputDir, "txt")
  val reader = new EidosSystem()

  // For each file in the input directory:
  files.par.foreach { file =>
    // 1. Open corresponding output file
    println(s"Extracting from ${file.getName}")
    val pw = FileUtils.printWriterFromFile(s"$outputDir/${file.getName}.jsonld")
    // 2. Get the input file contents
    val text = FileUtils.getTextFromFile(file)
    // 3. Extract causal mentions from the text
    val annotatedDocuments = Seq(reader.extractFromText(text))
    // 4. Convert to JSON
    val corpus = new JLDCorpus(annotatedDocuments, reader)
    val mentionsJSONLD = corpus.serialize()
    // 5. Write to output file
    pw.println(stringify(mentionsJSONLD, pretty = true))

    pw.close()
  }
}

object MakeMITRETablesFromDirectory extends App {
  val inputDir = args(0)
  val outputDir = args(1)
  val files = findFiles(inputDir, "txt")
  val reader = new EidosSystem()


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

  // MITRE Table
  val pw = new PrintWriter(s"$outputDir/MITRE_table.tsv")
  val nKeep: Int = 5 // how many of the groundings to print for each ontology
  // JSON-LD
  val pwJson = new PrintWriter(s"$outputDir/UA_10docCAG.jsonld")
  val annotatedDocs = new ArrayBuffer[AnnotatedDocument]

  // Header
  val header = "Source\tSystem\tSentence ID\tFactor A Text\tFactor A Normalization\t" +
    "Factor A Modifiers\tFactor A Polarity\tRelation Text\tRelation Normalization\t" +
    "Relation Modifiers\tFactor B Text\tFactor B Normalization\tFactor B Modifiers\t" +
    "Factor B Polarity\tLocation\tTime\tEvidence\t" +
    "Factor A top5_UNOntology\tFactor A top5_FAOOntology\tFactor A top5_WDIOntology" +
    "Factor B top5_UNOntology\tFactor B top5_FAOOntology\tFactor B top5_WDIOntology"
  pw.println(header)

  // For each file in the input directory:
  files.par.foreach { file =>
    // 1. Open corresponding output file
    println(s"Extracting from ${file.getName}")

    // 2. Get the input file contents
    val text = FileUtils.getTextFromFile(file)
    //val lines = FileUtils.getCommentedLinesFromSource(Sourcer.sourceFromFile(file))
    // 3. Extract causal mentions from the text
    val annotatedDocuments = Seq(reader.extractFromText(text))


    for {
      annotatedDocument <- annotatedDocuments
      mentionsToPrint = annotatedDocument.eidosMentions.filter(m => reader.releventEdge(m.odinMention))
      mention <- mentionsToPrint

      source = file.getName
      system = "Eidos"
      sentence_id = mention.odinMention.sentence

      cause <- mention.asInstanceOf[EidosEventMention].eidosArguments("cause")

      factor_a_txt = cause.odinMention.text
      factor_a_norm = getBaseGrounding(cause)
      factor_a_modifier = getModifier(cause)
      factor_a_polarity = getPolarity(cause)
      factor_a_un = getGroundingsString(cause, EidosOntologyGrounder.UN_NAMESPACE, nKeep)
      factor_a_fao = getGroundingsString(cause, EidosOntologyGrounder.FAO_NAMESPACE, nKeep)
      factor_a_wdi = getGroundingsString(cause, EidosOntologyGrounder.WDI_NAMESPACE, nKeep)

      trigger = mention.odinMention.asInstanceOf[EventMention].trigger
      relation_txt = trigger.text
      relation_norm = mention.label // i.e., "Causal" or "Correlation"
      relation_modifier = getModifier(mention) // prob none

      effect <- mention.asInstanceOf[EidosEventMention].eidosArguments("effect")
      factor_b_txt = effect.odinMention.text
      factor_b_norm = getBaseGrounding(effect)
      factor_b_modifier = getModifier(effect)
      factor_b_polarity = getPolarity(effect)
      factor_b_un = getGroundingsString(effect, EidosOntologyGrounder.UN_NAMESPACE, nKeep)
      factor_b_fao = getGroundingsString(effect, EidosOntologyGrounder.FAO_NAMESPACE, nKeep)
      factor_b_wdi = getGroundingsString(effect, EidosOntologyGrounder.WDI_NAMESPACE, nKeep)

      location = "" // I could try here..?
      time = ""
      evidence = mention.odinMention.sentenceObj.getSentenceText.trim

      row = source + "\t" + system + "\t" + sentence_id + "\t" +
        factor_a_txt + "\t" + factor_a_norm + "\t" + factor_a_modifier + "\t" + factor_a_polarity + "\t" +
        relation_txt + "\t" + relation_norm + "\t" + relation_modifier + "\t" +
        factor_b_txt + "\t" + factor_b_norm + "\t" + factor_b_modifier + "\t" + factor_b_polarity + "\t" +
        location + "\t" + time + "\t" + evidence + "\t" +
        factor_a_un + "\t" + factor_b_fao + "\t" + factor_a_wdi + "\t" +
        factor_b_un + "\t" + factor_b_fao + "\t" + factor_b_wdi

    } {
      pw.println(row)
      annotatedDocs.append(annotatedDocument)
    }
  }

  // Write the JSONLD with all accumulated AnnotatedDocs
  FileUtils.writeToJSONLD(annotatedDocs, pwJson, reader)

  // Housekeeping
  pw.close()
  pwJson.close()
}