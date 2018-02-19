package org.clulab.wm
import java.io.PrintWriter

import agro.demo.examples.ExtractFromText.{annotatedDocument, reader}
import org.clulab.serialization.json.stringify
import org.clulab.wm.serialization.json.{JLDCorpus, WMJSONSerializer}
import org.clulab.wm.wmutils.FileUtils.findFiles
import org.json4s.jackson.JsonMethods._


object ExtractFromDirectory extends App {

  val reader = new EidosSystem()
  val inputDir = args(0)
  val outputDir = args(1)
  val files = findFiles(inputDir, "txt")

  for {
    // For each file in the input directory:
    file <- files
    // 1. Open corresponding output file
    outputFile = file.getName
    pw = new PrintWriter(s"$outputDir/$outputFile.jsonld")
    // 2. Get the input file contents
    source = scala.io.Source.fromFile(file)
    text <- source.getLines()
    // 3. Extract causal mentions from the text
    annotatedDocument = reader.extractFrom(text)
    // 4. Convert to JSON
    corpus = new JLDCorpus(Seq(annotatedDocument), reader)
    mentionsJSONLD = corpus.serialize()
  } {
    // 5. Write to output file and close
    pw.println(stringify(mentionsJSONLD, pretty = true))
    pw.close()
  }

}