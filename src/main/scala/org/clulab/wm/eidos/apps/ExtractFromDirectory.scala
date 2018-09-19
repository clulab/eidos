package org.clulab.wm.eidos.apps

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.{FileUtils, Timer}

object ExtractFromDirectory extends App {
  def work () {
    val inputDir = args(0)
    val outputDir = args(1)

    val files = findFiles(inputDir, "txt")
    val reader = Timer.time("Startup") {
      val reader = new EidosSystem()
      val text = "This is a test made yesterday or the day before."
      val annotatedDocuments = Seq(reader.extractFromText(text))
      val corpus = new JLDCorpus(annotatedDocuments, reader)
      val mentionsJSONLD = corpus.serialize()
      reader
    }

    Timer.time("Process the documents") {
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
  }

  work()
}
