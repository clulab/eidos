package org.clulab.wm.eidos.apps

import org.clulab.processors.Document
import org.clulab.serialization.DocumentSerializer
import org.clulab.serialization.json.{JSONSerializer, stringify}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.json4s.jackson.JsonMethods

object ExtractDocumentFromDirectory extends App {

  def annotateTxt(reader: EidosSystem, text: String): AnnotatedDocument = {
    reader.extractFromText(text)
  }

  protected def annotateDocument(reader: EidosSystem, document: Document): AnnotatedDocument = {
    val eidosDoc = reader.annotateDoc(document)

    reader.extractFromDoc(eidosDoc)
  }

  def annotateJson(reader: EidosSystem, text: String): AnnotatedDocument = {
    val doc: Document = JSONSerializer.toDocument(JsonMethods.parse(text))

    annotateDocument(reader, doc)
  }

  def annotateDoc(reader: EidosSystem, text: String): AnnotatedDocument = {
    val doc: Document = (new DocumentSerializer).load(text)

    annotateDocument(reader, doc)
  }

  val inputDir = args(0)
  val outputDir = args(1)
  val format = if (args.length > 2) args(2) else "1"
  val extension = if (args.length > 3) args(3) else "txt"
  val annotator = format match {
    case "1" => annotateTxt _
    case "2" => annotateJson _
    case "3" => annotateDoc _
    case _ => throw new Exception(s"Unknown format '$format'")
  }
  val files = findFiles(inputDir, extension)
  val reader = new EidosSystem()

  // For each file in the input directory:
  files.par.foreach { file =>
    // 1. Open corresponding output file
    println(s"Extracting from ${file.getName}")
    FileUtils.printWriterFromFile(s"$outputDir/${file.getName}.jsonld").autoClose { pw =>
      // 2. Get the input file contents
      val text = FileUtils.getTextFromFile(file)
      // 3. Extract causal mentions from the text
      val annotatedDocuments = Seq(annotator(reader, text))
      // 4. Convert to JSON
      val corpus = new JLDCorpus(annotatedDocuments)
      val mentionsJSONLD = corpus.serialize()
      // 5. Write to output file
      pw.println(stringify(mentionsJSONLD, pretty = true))
    }
  }
}
