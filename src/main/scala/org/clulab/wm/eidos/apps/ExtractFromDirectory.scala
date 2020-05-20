package org.clulab.wm.eidos.apps

import java.io.File

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.jsonld.JLDCorpus
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Timer

object ExtractFromDirectory extends App {
  val inputDir = args(0)
  val outputDir = args(1)

  require(new File(outputDir).isDirectory, s"The output directory '$outputDir' should be an existing directory.")

  val files = FileUtils.findFiles(inputDir, "txt")
  val eidosSystem = new EidosSystem

  // For each file in the input directory
  files.foreach { file =>
    println(s"Extracting from ${file.getName}")
    // 1. Get the input file contents
    val text = FileUtils.getTextFromFile(file)
    // 2. Extract causal mentions from the text
    val annotatedDocument = eidosSystem.extractFromText(text)
    // 3. Write JSON to the output file
    FileUtils.printWriterFromFile(s"$outputDir/${file.getName}.jsonld").autoClose { printWriter =>
      new JLDCorpus(annotatedDocument).serialize(printWriter)
    }
  }
  Timer.summarize
}
