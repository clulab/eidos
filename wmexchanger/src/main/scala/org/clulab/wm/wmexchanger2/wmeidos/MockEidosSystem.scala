package org.clulab.wm.wmexchanger2.wmeidos

import org.clulab.processors.Document
import org.clulab.struct.Interval
import org.clulab.wm.eidos.EidosOptions
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.groundings.OntologyAliases
import org.clulab.wm.eidos.groundings.OntologyHandler
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.metadata.Metadata
import org.clulab.wm.eidos.serialization.jsonld.JLDDeserializer
import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.StringUtils

import java.io.File

class MockEidosSystem(mockDir: String) extends EidosSystemish {
  val annotatedDocument = {
    val mockFile = FileEditor(new File("reading.jsonld")).setDir(mockDir).get
    val json = FileUtils.getTextFromFile(mockFile)
    val deserializer = new JLDDeserializer()
    val annotatedDocument = deserializer.deserialize(json).head

    annotatedDocument
  }

  def getEmptyAnnotatedDocument(idOpt: Option[String]): AnnotatedDocument = {
    val document = new Document(Array.empty)
    document.id = idOpt

    val annotatedDocument = AnnotatedDocument(document, Seq.empty)

    annotatedDocument
  }

  def extractFromText(text: String, options: EidosOptions, metadata: Metadata): AnnotatedDocument = {
    Thread.sleep(100)
    annotatedDocument
  }

  def newOntologyHandler(file: File): OntologyHandler = {
    val version = StringUtils.beforeFirst(file.getName, '.')

    new MockOntologyHandler(version)
  }
}

class MockOntologyHandler(version: String) extends OntologyHandler(null, null, null, null, false, None, None) {

  override def ground(eidosMention: EidosMention): EidosMention = {
    // TODO: Could replace the version on the grounding.
    eidosMention
  }

  override def reground(sentenceText: String, interval: Interval, document: Document): OntologyAliases.OntologyGroundingMap = ???
  override def reground(sentenceText: String, interval: Interval): OntologyAliases.OntologyGroundingMap = ???
  override def reground(name: String = "Custom", ontologyYaml: String, texts: Seq[String], filter: Boolean = true, topk: Int = 10, isAlreadyCanonicalized: Boolean = true): Array[Array[(String, Float)]] = ???
}
