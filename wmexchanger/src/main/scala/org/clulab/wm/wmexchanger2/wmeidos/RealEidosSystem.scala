package org.clulab.wm.wmexchanger2.wmeidos

import org.clulab.processors.Document
import org.clulab.wm.eidos.EidosOptions
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.groundings.OntologyHandler
import org.clulab.wm.eidos.groundings.grounders.EidosOntologyGrounder
import org.clulab.wm.eidos.metadata.Metadata
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.StringUtils
import org.clulab.wm.ontologies.NodeTreeDomainOntologyBuilder

import java.io.File
import java.io.FileInputStream

class RealEidosSystem extends EidosSystemish {
  val oldEidosSystem = new EidosSystem()
  val oldComponents = oldEidosSystem.components
  val oldTokenizer = oldComponents.procOpt.get.getTokenizer
  val oldOntologyHandler = oldComponents.ontologyHandlerOpt.get
  val oldSentencesExtractor = oldOntologyHandler.sentencesExtractor
  val oldCanonicalizer = oldOntologyHandler.canonicalizer
  val eidosSystem = {
    val newComponents = oldComponents.copy(ontologyHandlerOpt = None)
    new EidosSystem(newComponents)
  }

  def getEmptyAnnotatedDocument(idOpt: Option[String]): AnnotatedDocument = {
    val document = new Document(Array.empty)
    document.id = idOpt

    val annotatedDocument = AnnotatedDocument(document, Seq.empty)

    annotatedDocument
  }

  def extractFromText(text: String, options: EidosOptions, metadata: Metadata): AnnotatedDocument = {
    eidosSystem.extractFromText(text, options, metadata)
  }

  def newOntologyHandler(file: File): OntologyHandler = {
    val version = StringUtils.beforeFirst(file.getName, '.')
    val newDomainOntologyBuilder = new NodeTreeDomainOntologyBuilder(oldSentencesExtractor, oldCanonicalizer, filtered = true)
    val newDomainOntology = (new FileInputStream(file)).autoClose { inputStream =>
      newDomainOntologyBuilder.buildFromStream(inputStream, Some(version), None)
    }
    val newOntologyGrounder = EidosOntologyGrounder.mkGrounder(EidosOntologyGrounder.PRIMARY_NAMESPACE, newDomainOntology, oldOntologyHandler.wordToVec, oldOntologyHandler.canonicalizer, oldTokenizer)
    val newOntologyGrounders = oldOntologyHandler.ontologyGrounders.map { ontologyGrounder =>
      if (ontologyGrounder.name == EidosOntologyGrounder.PRIMARY_NAMESPACE) newOntologyGrounder
      else ontologyGrounder
    }
    val newOntologyHandler = new OntologyHandler(
      newOntologyGrounders,
      oldOntologyHandler.wordToVec,
      oldOntologyHandler.sentencesExtractor,
      oldOntologyHandler.canonicalizer,
      oldOntologyHandler.includeParents,
      oldOntologyHandler.topN,
      oldOntologyHandler.threshold
    )

    newOntologyHandler
  }
}
