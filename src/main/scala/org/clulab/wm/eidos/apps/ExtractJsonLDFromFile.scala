package org.clulab.wm.eidos.apps

import java.io.PrintWriter

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.{FileUtils, Sinker, Sourcer}

object ExtractJsonLDFromFile extends App {
  val inputFile = args(0)
  val outputFile = if (args.size > 1) args(1) else args(0) + ".jsonld"
  lazy val reader = new EidosSystem()

  // 1. Get the input file contents
  val text = FileUtils.getTextFromFile(inputFile)
  // 2. Open corresponding output file
  val pw = Sinker.printWriterFromFile(outputFile)
  // 3. Extract causal mentions from the text
  val annotatedDocument = reader.extractFromText(text)
  // 4. Convert to an object that can be serialized as desired
  val corpus = new JLDCorpus(Seq(annotatedDocument), reader)
  // 5. Convert to JSON
  val mentionsJSONLD = corpus.serialize()
  // 6. Write to output file and close it
  pw.println(stringify(mentionsJSONLD, pretty = true))
  pw.close()
}
