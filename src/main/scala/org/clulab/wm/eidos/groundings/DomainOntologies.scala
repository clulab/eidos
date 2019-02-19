package org.clulab.wm.eidos.groundings

import org.clulab.processors.Processor
import org.clulab.wm.eidos.groundings.TreeDomainOntology.TreeDomainOntologyBuilder
import org.clulab.wm.eidos.utils.Canonicalizer
import org.slf4j.LoggerFactory

object DomainOntologies {
  protected lazy val logger = LoggerFactory.getLogger(this.getClass())

  def apply(ontologyPath: String, serializedPath: String, proc: Processor, canonicalizer: Canonicalizer, filter: Boolean = true, useCache: Boolean = false): DomainOntology = {
    if (useCache) {
      logger.info(s"Processing cached yml ontology ${serializedPath}...")
      CompactDomainOntology.load(serializedPath)
    }
    else {
      logger.info(s"Processing yml ontology ${ontologyPath}..")
      new TreeDomainOntologyBuilder(proc, canonicalizer, filter).buildFromPath(ontologyPath)
    }
  }

}
