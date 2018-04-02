package org.clulab.wm.eidos.apps

import java.io.{PrintWriter, File}

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sourcer

import scala.collection.Seq

object ExtractFromFile2 extends App {
  val inputFile = args(0)
  //val outputDir = args(1)
  //val files = findFiles(inputDir, "txt")
  val reader = new EidosSystem()

   val file = new File(inputFile)
    // 1. Open corresponding output file
    println(s"Extracting from ${file.getName}")
    val pw = new PrintWriter(s"${file.getName}.jsonld")
    // 2. Get the input file contents
    val lines = FileUtils.getCommentedLinesFromSource(Sourcer.sourceFromFile(file))
    // 3. Extract causal mentions from the text
    val annotatedDocuments = lines.map(reader.extractFromText(_))
    // 4. Convert to JSON
    val corpus = new JLDCorpus(annotatedDocuments, reader)
    val mentionsJSONLD = corpus.serialize()
    // 5. Write to output file
    pw.println(stringify(mentionsJSONLD, pretty = true))
    pw.close()
}
