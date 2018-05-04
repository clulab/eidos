package org.clulab.wm.eidos.apps

import java.io.PrintWriter

import org.clulab.odin.EventMention
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.attachments.{Decrease, EidosAttachment, Increase, Quantification}
import org.clulab.wm.eidos.groundings.Aliases.Groundings
import org.clulab.wm.eidos.groundings.{EidosOntologyGrounder, OntologyGrounding}
import org.clulab.wm.eidos.mentions.{EidosEventMention, EidosMention}
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.{DisplayUtils, FileUtils, Sourcer}
import org.clulab.wm.eidos.utils.GroundingUtils._

object ExtractFromDirectory extends App {
  val inputDir = args(0)
  val outputDir = args(1)
  val files = findFiles(inputDir, "txt")
  val reader = new EidosSystem()

  // For each file in the input directory:
  files.par.foreach { file =>
    // 1. Open corresponding output file
    println(s"Extracting from ${file.getName}")
    val pw = new PrintWriter(s"$outputDir/${file.getName}.jsonld")
    // 2. Get the input file contents
    val lines = FileUtils.getCommentedLinesFromSource(Sourcer.sourceFromFile(file))
    // 3. Extract causal mentions from the text
    val annotatedDocuments = lines.map(reader.extractFromText(_))
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
    val attachments = mention.odinArguments.values.toSeq.flatten.flatMap(argMention => argMention.attachments)
    val quantTriggers = attachments
      .filter(a => a.isInstanceOf[Quantification])
      .map(quant => quant.asInstanceOf[Quantification].trigger)
      .map(t => t.toLowerCase)

    if (quantTriggers.nonEmpty) s"${quantTriggers.mkString(", ")}" else ""
  }

  //fixme: not working -- always ;
  def getPolarity(mention: EidosMention): String = {
    val sb = new StringBuilder
    val attachments = mention.odinMention.attachments
    val incTriggers = attachments.filter(a => a.isInstanceOf[Increase]).map(inc => inc.asInstanceOf[Increase].trigger)
    val decTriggers = attachments.filter(a => a.isInstanceOf[Decrease]).map(inc => inc.asInstanceOf[Decrease].trigger)
    for (t <- incTriggers) sb.append(s"Increase(${t})")
    for (t <- decTriggers) sb.append(s"Decrease(${t})")

    sb.mkString(", ")
  }

  val pw = new PrintWriter(s"$outputDir/MITRE_table.tsv")
  val nKeep: Int = 5 // how many of the groundings to print for each ontology

  // For each file in the input directory:
  files.par.foreach { file =>
    // 1. Open corresponding output file
    println(s"Extracting from ${file.getName}")

    // 2. Get the input file contents
    val lines = FileUtils.getCommentedLinesFromSource(Sourcer.sourceFromFile(file))

    // 4. Convert to tsv TODO
    val header = "Source\tSystem\tSentence ID\tFactor A Text\tFactor A Normalization\t" +
      "Factor A Modifiers\tFactor A Polarity\tRelation Text\tRelation Normalization\t" +
      "Relation Modifiers\tFactor B Text\tFactor B Normalization\tFactor B Modifiers\t" +
      "Factor B Polarity\tLocation\tTime\tEvidence\t" +
      "Factor A top5_UNOntology\tFactor A top5_FAOOntology\tFactor A top5_WDIOntology" +
      "Factor B top5_UNOntology\tFactor B top5_FAOOntology\tFactor B top5_WDIOntology"
    pw.println(header)

    for {
      line <- lines
      annotatedDocument = reader.extractFromText(line)
      mentionsToPrint = annotatedDocument.eidosMentions.filter(m => reader.releventEdge(m.odinMention))
      mention <- mentionsToPrint

      source = file.getName
      system = "EIDOS"
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
      evidence = mention.odinMention.sentenceObj.getSentenceText().trim

      row = source + "\t" + system + "\t" + sentence_id + "\t" +
        factor_a_txt + "\t" + factor_a_norm + "\t" + factor_a_modifier + "\t" + factor_a_polarity + "\t" +
        relation_txt + "\t" + relation_norm + "\t" + relation_modifier + "\t" +
        factor_b_txt + "\t" + factor_b_norm + "\t" + factor_b_modifier + "\t" + factor_b_polarity + "\t" +
        location + "\t" + time + "\t" + evidence + "\t" +
        factor_a_un + "\t" + factor_b_fao + "\t" + factor_a_wdi + "\t" +
        factor_b_un + "\t" + factor_b_fao + "\t" + factor_b_wdi
    //"top5_UNOntology\ttop5_FAOOntology\ttop5_WDIOntology"

    } pw.println(row)
  }

  // Housekeeping
  pw.close()
}