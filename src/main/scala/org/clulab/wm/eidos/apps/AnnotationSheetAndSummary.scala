package org.clulab.wm.eidos.apps

import java.io.File
import java.util.Calendar
import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.struct.Counter
import org.clulab.utils.Configured
import org.clulab.utils.ThreadUtils
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.exporters.GroundingAnnotationExporter
import org.clulab.wm.eidos.serialization.jsonld.JLDDeserializer
import org.clulab.wm.eidoscommon.EidosParameters
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.{CsvWriter, FileUtils}

import scala.collection.Seq

object AnnotationSheetAndSummary extends App with Configured {

  val config = ConfigFactory.load("eidos")
  override def getConf: Config = config

  collection.parallel.ForkJoinTasks.defaultForkJoinPool
  lazy val reader = new EidosSystem()
  lazy val deserializer = new JLDDeserializer()
  val groundAs = getArgStrings("apps.groundAs", None)
  val topN = getArgInt("apps.groundTopN", Some(5))
  val exporter = new GroundingAnnotationExporter("", reader, groundAs, topN)

  val inputDir = getArgString("apps.inputDirectory", None)
  val outputDir = getArgString("apps.outputDirectory", None)
  val inputExtension = getArgString("apps.inputFileExtension", None)
  val nCores = getArgInt("apps.nCores", Some(1))

  val headers2: Seq[String] = Seq(
    "RULE",
    "COUNT of RULE",
    "% of all",
    "Num correct",
    "Num incorrect",
    "% correct",
    "% curated"
  )


  // handle text or previously generated jsonld
  def getInput(f: File): Option[AnnotatedDocument] = {
    inputExtension match {
      case j if j.endsWith("jsonld") || j.endsWith("json") =>
        val json = FileUtils.getTextFromFile(f)
        try {
          Some(deserializer.deserialize(json).head)
        } catch {
          case throwable: Throwable =>
            println(s"Failed to parse: ${f.getCanonicalPath} --> $throwable")
            None
        }
      case _ =>
        val text = FileUtils.getTextFromFile(f)
        Some(reader.extractFromText(text))
    }
  }

  val files = FileUtils.findFiles(inputDir, inputExtension)
  val parFiles = ThreadUtils.parallelize(files, nCores)
  val annotatedDocuments = parFiles.map(getInput)

  // For each file in the input directory:
  val relevantMentions = parFiles.seq
    // extract the annotated documents from the files
    .map(f => getInput(f))
    // filter the eidos mentions in each to be only relevant ones
    .flatMap{ adOpt =>
      adOpt match {
        case Some(ad) => reader.components.stopwordManagerOpt.get.relevantMentions(ad).filter(_.label == EidosParameters.CAUSAL_LABEL)
        case None => Seq()
      }
    }

  // tally the rules in the found mentions
  val ruleCounter = new Counter[String]
  relevantMentions.foreach(m => ruleCounter.incrementCount(m.odinMention.foundBy))

  // make a row for the sheet for each EidosMention
  val rows = exporter.getRows(relevantMentions)
  val shuffledRows = scala.util.Random.shuffle(rows)

  val timestamp = Calendar.getInstance.getTime

  new CsvWriter(FileUtils.printWriterFromFile(s"$outputDir/rule_annotation.tsv")).autoClose { csvWriter1 =>
    new CsvWriter(FileUtils.printWriterFromFile(s"$outputDir/rule_summary.tsv")).autoClose { csvWriter2 =>

      // Sheet 1 -- Extraction Data
      csvWriter1.println(s"EIDOS Rule Annotation -- generated $timestamp")
      exporter.printHeader(csvWriter1)
      shuffledRows.foreach(csvWriter1.println)
      // Sheet 2 -- Summary Statistics
      csvWriter2.println(headers2)
      val summaryRows = AnnotationTSV.counterToRows(ruleCounter, ruleColumn = "Q", correctColumn = "L")
      summaryRows.foreach(csvWriter2.println)
    }
  }
}
