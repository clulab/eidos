package org.clulab.wm.eidos.apps


import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.utils.Configured
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.CompositionalGrounder
import org.clulab.wm.eidos.utils.{ExportUtils, FileUtils}

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
  val groundedAs = groundAs.flatMap { format =>
    format match {
      case "wm_compositional" =>
        CompositionalGrounder.branches.map { subformat => format + "/" + subformat }
      case other => Seq(other)
    }
  }

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
    val annotatedDocuments = Seq(reader.extractFromText(text, id = Some(file.getName)))
    // 4. Export to all desired formats
    exportAs.foreach { format =>
      ExportUtils.getExporter(format, s"$outputDir/${file.getName}", reader, groundedAs, topN).export(annotatedDocuments)
    }
  }
}

