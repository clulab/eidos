package org.clulab.wm.eidos.apps

import com.typesafe.config.Config

import org.clulab.utils.Configured
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.exporters.Exporter
import org.clulab.wm.eidos.serialization.jsonld.JLDDeserializer
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils

/**
  * App used to extract mentions from files in a directory and produce the desired output format (i.e., jsonld, mitre
  * tsv or serialized mentions).  The input directory nd output file as well as the desired export formats are specified
  * in eidos.conf (located in src/main/resources).
  */
object ReconstituteCombineAndExport extends App with Configured {
  val config = EidosSystem.defaultConfig
  override def getConf: Config = config

  val inputDir = getArgString("apps.inputDirectory", None)
  val outputFile = getArgString("apps.outputFile", None)
  val inputExtension = ".jsonld"
  val deserializer = new JLDDeserializer()
  val exportAs = getArgStrings("apps.exportAs", None)
  val groundAs = getArgStrings("apps.groundAs", None)
  val topN = getArgInt("apps.groundTopN", Some(5))
  val files = FileUtils.findFiles(inputDir, inputExtension)
  val reader: EidosSystem = null // new EidosSystem()

  FileUtils.printWriterFromFile(outputFile).autoClose { printWriter =>
    val exporters = exportAs.map { format =>
      Exporter(format, printWriter, reader, groundAs, topN)
    }

    // For each file in the input directory:
    files.foreach { file =>
      // 1. Open corresponding output file and make all desired exporters
      println(s"Extracting from ${file.getName}")
      // 2. Get the input file contents (extractions)
      val json = FileUtils.getTextFromFile(file)
      val annotatedDocument = deserializer.deserialize(json).head
      // 3. Export to all desired formats
      exporters.foreach { exporter =>
        exporter.export(annotatedDocument)
      }
    }
  }
}
