package org.clulab.wm.eidos.apps

import java.io.PrintWriter
import scala.collection.Seq
import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.odin.JLDCorpus
import org.clulab.wm.eidos.utils.FileUtils.findFiles

object ExtractFromDirectory extends App {
  val reader = new EidosSystem()
  val inputDir = args(0)
  val outputDir = args(1)
  val files = findFiles(inputDir, "txt")

  // For each file in the input directory:
  files foreach { file =>
    // 1. Open corresponding output file
    val pw = new PrintWriter(s"$outputDir/${file.getName}.jsonld")
    // 2. Get the input file contents
    val source = scala.io.Source.fromFile(file)
    source.getLines() foreach { text =>
      // 3. Extract causal mentions from the text
      val annotatedDocument = reader.extractFrom(text)
      // 4. Convert to JSON
      val corpus = new JLDCorpus(Seq(annotatedDocument), reader)
      val mentionsJSONLD = corpus.serialize()
      // 5. Write to output file
      pw.println(stringify(mentionsJSONLD, pretty = true))
    }
    pw.close()
  }
}