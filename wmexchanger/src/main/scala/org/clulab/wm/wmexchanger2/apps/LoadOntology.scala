package org.clulab.wm.wmexchanger2.apps

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.StringUtils
import org.clulab.wm.ontologies.NodeTreeDomainOntology
import org.clulab.wm.ontologies.NodeTreeDomainOntologyBuilder

import java.io.File
import java.io.FileInputStream

object LoadOntology extends App {
  val ontologyFile = new File(args(0))

  def newDomainOntology(eidosSystem: EidosSystem, file: File, version: String): NodeTreeDomainOntology = {
    val oldComponents = eidosSystem.components
    val oldOntologyHandler = oldComponents.ontologyHandlerOpt.get
    val oldSentencesExtractor = oldOntologyHandler.sentencesExtractor
    val oldCanonicalizer = oldOntologyHandler.canonicalizer

    val newDomainOntologyBuilder = new NodeTreeDomainOntologyBuilder(oldSentencesExtractor, oldCanonicalizer, filtered = true)
    val newDomainOntology = (new FileInputStream(file)).autoClose { inputStream =>
      newDomainOntologyBuilder.buildFromStream(inputStream, Some(version), None)
    }

    newDomainOntology
  }

  val eidosSystem = new EidosSystem()
  val version = StringUtils.beforeFirst(ontologyFile.getName, '.')
  newDomainOntology(eidosSystem, ontologyFile, version)
}
