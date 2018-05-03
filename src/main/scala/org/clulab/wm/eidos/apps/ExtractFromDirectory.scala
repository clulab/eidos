package org.clulab.wm.eidos.apps

import java.io.PrintWriter

import org.clulab.odin.EventMention
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.apps.MakeMITRETablesFromDirectory.mentionsToPrint
import org.clulab.wm.eidos.attachments.{Decrease, EidosAttachment, Increase, Quantification}
import org.clulab.wm.eidos.mentions.{EidosEventMention, EidosMention}
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sourcer

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
    val quantTriggers = attachments.filter(a => a.isInstanceOf[Quantification]).map(quant => quant.asInstanceOf[Quantification].trigger)

    if (quantTriggers.nonEmpty) s"${quantTriggers.mkString(", ")})" else ""
  }

  def getPolarity(mention: EidosMention): String = {
    val attachments = mention.odinArguments.values.toSeq.flatten.flatMap(argMention => argMention.attachments)
    val incTriggers = attachments.filter(a => a.isInstanceOf[Increase]).map(inc => inc.asInstanceOf[Increase].trigger)
    val decTriggers = attachments.filter(a => a.isInstanceOf[Decrease]).map(inc => inc.asInstanceOf[Decrease].trigger)
    val incString = if (incTriggers.nonEmpty) s"Increase(${incTriggers.mkString(", ")})" else ""
    val decString = if (decTriggers.nonEmpty) s"Decrease(${decTriggers.mkString(", ")})" else ""

    s"$incString; $decString"
  }

  val pw = new PrintWriter(s"$outputDir/MITRE_table.tsv")

  // For each file in the input directory:
  files.par.foreach { file =>
    // 1. Open corresponding output file
    println(s"Extracting from ${file.getName}")

    // 2. Get the input file contents
    val lines = FileUtils.getCommentedLinesFromSource(Sourcer.sourceFromFile(file))


    // 4. Convert to tsv TODO
    val head = "Source\tSystem\tSentence ID\tFactor A Text\tFactor A Normalization\t" +
      "Factor A Modifiers\tFactor A Polarity\tRelation Text\tRelation Normalization\t" +
      "Relation Modifiers\tFactor B Text\tFactor B Normalization\tFactor B Modifiers\t" +
      "Factor B Polarity\tLocation\tTime\tEvidence"


    val res = for {
      line <- lines
      annotatedDocument = reader.extractFromText(line)
      mentionsToPrint = annotatedDocument.eidosMentions.filter(m => reader.releventEdge(m.odinMention))

//      for {
      mention <- mentionsToPrint

      source = file.getName
      system = "EIDOS"
      sentence_id = mention.odinMention.sentence

      cause <- mention.asInstanceOf[EidosEventMention].eidosArguments("cause")

      factor_a_txt = cause.odinMention.text
      factor_a_norm = cause.canonicalName
      factor_a_modifier = getModifier(cause)
      factor_a_polarity = getPolarity(cause)

      trigger = mention.odinMention.asInstanceOf[EventMention].trigger
      relation_txt = trigger.text
      relation_norm = mention.label // i.e., "Causal" or "Correlation"
      relation_modifier = getModifier(mention) // prob none

      effect <- mention.asInstanceOf[EidosEventMention].eidosArguments("effect")
      factor_b_txt = effect.odinMention.text
      factor_b_norm = effect.canonicalName
      factor_b_modifier = getModifier(effect)
      factor_b_polarity = getPolarity(effect)

      location = "" // I could try here
      time = ""
      evidence = mention.text

      row = source + "\t" + system + "\t" + sentence_id + "\t" + factor_a_txt + "\t" + factor_a_norm + "\t" +
        factor_a_modifier + "\t" + factor_a_polarity + "\t" + relation_txt + "\t" + relation_norm + "\t" +
        relation_modifier + "\t" + factor_b_txt + "\t" + factor_b_norm + "\t" + factor_b_modifier + "\t" +
        factor_b_polarity + "\t" + location + "\t" + time + "\t" + evidence
    //} //yield row
    //      rows = head ??? rows
    //      // 5. Write to output file
    //      rows.foreach(row => pw.println(row))

    } yield 0


    //    // 3. Extract causal mentions from the text
    //    val annotatedDocuments = lines.map(reader.extractFromText(_))
    //    // keep only causal and maybe correlation!!!
    //    val mentionsToPrint = annotatedDocuments.map(_.eidosMentions)//.filter(_.label == "Causal")



  }
  pw.close()
}