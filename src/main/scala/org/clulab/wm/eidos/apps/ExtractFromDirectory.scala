package org.clulab.wm.eidos.apps

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.EidosSystem
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
