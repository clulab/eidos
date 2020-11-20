package org.clulab.wm.eidos.apps

import ai.lum.common.StringUtils.StringWrapper
import com.typesafe.config.{Config, ConfigFactory}
import java.util.Calendar

import org.clulab.odin.EventMention
import org.clulab.struct.Counter
import org.clulab.utils.Configured
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.exporters.EntityInfo
import org.clulab.wm.eidos.exporters.Exporter
import org.clulab.wm.eidos.mentions.EidosEventMention
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.TsvWriter

import scala.collection.Seq

object AnnotationTSV extends App with Configured {

  // Methods for Sheet 1
  val headers1: Seq[String] = Seq(
    "Source",
    "Sentence ID",

    "Factor A Text",
    "Factor A Normalization",
    "Factor A Modifiers",
    "Factor A Polarity",

    "Relation Text",
    "Relation Normalization",
    "Relation Modifiers",

    "Factor B Text",
    "Factor B Normalization",
    "Factor B Modifiers",
    "Factor B Polarity",

    "Location",
    "Time",
    "Evidence",

    "Extraction Correct",
    "Annotator",
    "Rule"
  )
  // Rule is index 18 = R
  // Correct is 16 = P

  def mkTableRows(annotatedDocument: AnnotatedDocument, filename: String, reader: EidosSystem): (Seq[Seq[AnyRef]], Counter[String]) = {
    val mentionsToPrint = reader.components.stopwordManager.relevantMentions(annotatedDocument)

    val ruleCounter = new Counter[String]

    val rows: Seq[Seq[AnyRef]] = for {
      mention <- mentionsToPrint

      source = filename
      sentence_id = mention.odinMention.sentence

      // Currently we're not going to output CrossSentenceMentions
      if mention.isInstanceOf[EidosEventMention]

      cause <- mention.asInstanceOf[EidosEventMention].eidosArguments("cause")
      factor_a_info = EntityInfo(cause, groundAs)
      trigger = mention.odinMention.asInstanceOf[EventMention].trigger

      effect <- mention.asInstanceOf[EidosEventMention].eidosArguments("effect")
      factor_b_info = EntityInfo(effect, groundAs)
      foundby = mention.odinMention.foundBy
      _ = ruleCounter.incrementCount(foundby) // a hack

    } yield Seq(
      source,
      sentence_id.toString
    ) ++
    factor_a_info.toTSV ++
    Seq(
      trigger.text.normalizeSpace, // Relation Text
      mention.label, // Relation Normalization, i.e., "Causal" or "Correlation",
      Exporter.getModifier(mention) // Relation Modifiers, prob none
    ) ++
    factor_b_info.toTSV ++
    Seq(
      "", // Location, I could try here..?,
      "", // Time
      mention.odinMention.sentenceObj.getSentenceText.normalizeSpace, // Evidence
      " ", // Extraction Correct
      " ", // Annotator
      foundby // Rule
    )

    (rows, ruleCounter)
  }

  // Methods for Sheet 2
  val headers2: Seq[String] = Seq(
    "RULE",
    "COUNT of RULE",
    "% of all",
    "Num correct",
    "Num incorrect",
    "% correct",
    "% curated"
  )

  // Sorted...
  def counterToRows(ruleCounter: Counter[String], ruleColumn: String = "S", correctColumn: String = "Q"): Seq[Seq[String]] = {
    val total = ruleCounter.getTotal
    val rows = ruleCounter.toSeq
      .sortBy(- _._2)
      .zipWithIndex
      .map(ruleInfo => ruleRow(ruleInfo._1._1, ruleInfo._1._2, total, ruleInfo._2, ruleColumn, correctColumn))
    rows :+ Seq("Grand Total", total.toString)
  }

  def ruleRow(rule: String, count: Double, total: Double, i: Int, ruleColumn: String, correctColumn: String): Seq[String] = {
    val j = i + 2 // account for the header and 1-indexing of google sheets
    val percAll = count / total
    val numCorrect = "=SUMIF(rule_annotation!$" +
      ruleColumn + ":$" + ruleColumn + ",A" + j +
      ",rule_annotation!$" + correctColumn + ":$" +
      correctColumn + ")"
    val numIncorrect = s"=COUNTIFS(rule_annotation!${ruleColumn}"+"$3:"+ s"${ruleColumn},A$j,rule_annotation!${correctColumn}" + "$3:" + s"${correctColumn}," + """"<>1")"""
    val percCorr = s"=IF(D$j+E$j>0, D$j/(D$j+E$j), " + """"")"""
    val percCurated = s"=(D$j+E$j)/B$j"

    Seq(
      rule,
      count.toString,
      percAll.toString,
      numCorrect,
      numIncorrect,
      percCorr,
      percCurated
    )
  }

  val config = EidosSystem.defaultConfig
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
  val processed = for { file <- files.par
    // 1. Get the input file contents
    text = FileUtils.getTextFromFile(file)
    // 2. Extract causal mentions from the text
    annotatedDocument = reader.extractFromText(text)
    // 3. Process into table rows
    (rows, rulesFound) = mkTableRows(annotatedDocument, file.getName, reader)
  } yield (rows, rulesFound)

  val (rows, rulesCounters) = processed.seq.unzip

  val totalRulesFound = rulesCounters match {
    case first +: rest =>
      rest.foreach(first += _)
      first
    case _ => throw new RuntimeException("I expected for there to be some rule counters!")
  }

  val timestamp = Calendar.getInstance.getTime

  new TsvWriter(FileUtils.printWriterFromFile(s"$outputDir/rule_annotation.tsv")).autoClose { tsvWriter1 =>
    new TsvWriter(FileUtils.printWriterFromFile(s"$outputDir/rule_summary.tsv")).autoClose { tsvWriter2 =>

      // Sheet 1 -- Extraction Data
      tsvWriter1.println(s"EIDOS Rule Annotation -- generated $timestamp")
      tsvWriter1.println(headers1)
      rows.flatten.foreach(tsvWriter1.println)
      // Sheet 2 -- Summary Statistics
      tsvWriter2.println(headers2)
      val summaryRows = counterToRows(totalRulesFound)
      summaryRows.foreach(tsvWriter2.println)
    }
  }
}
