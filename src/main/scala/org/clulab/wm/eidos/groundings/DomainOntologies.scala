package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.SentencesExtractor
import org.clulab.wm.eidos.groundings.OntologyHandler.serializedPath
import org.clulab.wm.eidos.groundings.TreeDomainOntology.TreeDomainOntologyBuilder
import org.clulab.wm.eidos.utils.Canonicalizer
import org.slf4j.Logger
import org.slf4j.LoggerFactory

object DomainOntologies {
  protected lazy val logger: Logger = LoggerFactory.getLogger(getClass)

  def apply(ontologyPath: String, serializedPath: String, sentencesExtractor: SentencesExtractor, canonicalizer: Canonicalizer, filter: Boolean = true, useCache: Boolean = false): DomainOntology = {
    if (useCache) {
      logger.info(s"Processing cached yml ontology $serializedPath...")
      CompactDomainOntology.load(serializedPath)
    }
    else {
      logger.info(s"Processing yml ontology $ontologyPath...")
      new TreeDomainOntologyBuilder(sentencesExtractor, canonicalizer, filter).buildFromPath(ontologyPath)
    }
  }

  def mkDomainOntology(name: String, ontologyPath: String, sentenceExtractor: SentencesExtractor, canonicalizer: Canonicalizer, cacheDir: String, useCached: Boolean): DomainOntology = {
    val ontSerializedPath: String = serializedPath(name, cacheDir)
    DomainOntologies(ontologyPath, ontSerializedPath, sentenceExtractor, canonicalizer: Canonicalizer, filter = true, useCache = useCached)
  }

}
