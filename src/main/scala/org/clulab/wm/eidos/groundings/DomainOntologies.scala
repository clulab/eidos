package org.clulab.wm.eidos.groundings

import org.clulab.processors.Processor
import org.clulab.wm.eidos.groundings.TreeDomainOntology.TreeDomainOntologyBuilder
import org.clulab.wm.eidos.utils.Canonicalizer
import org.slf4j.LoggerFactory

object DomainOntologies {
  val logger = LoggerFactory.getLogger(this.getClass())

  def serializedPath(name: String, dir: String): String = s"$dir/$name.serialized"

  def apply(ontologyPath: String, serializedPath: String, proc: Processor, canonicalizer: Canonicalizer, filter: Boolean = true, useCache: Boolean = false): DomainOntology = {
    if (useCache) {
      logger.info(s"Processing cached yml ontology ${serializedPath}...")
      CompactDomainOntology.load(serializedPath)
    }
    else {
      logger.info(s"Processing yml ontology ${ontologyPath}..")
      new TreeDomainOntologyBuilder(ontologyPath, proc, canonicalizer, filter).build()
    }
  }
}

// These are just here for when behavior might have to start differing.
object UNOntology {
  def apply(ontologyPath: String, serializedPath: String, proc: Processor, canonicalizer: Canonicalizer, filter: Boolean = true, useCache: Boolean = false) =
      DomainOntologies(ontologyPath, serializedPath, proc, canonicalizer: Canonicalizer, filter, useCache)
}

object WDIOntology {
  def apply(ontologyPath: String, serializedPath: String, proc: Processor, canonicalizer: Canonicalizer, filter: Boolean = true, useCache: Boolean = false) =
      DomainOntologies(ontologyPath, serializedPath, proc, canonicalizer: Canonicalizer, filter, useCache)
}

object FAOOntology {
  def apply(ontologyPath: String, serializedPath: String, proc: Processor, canonicalizer: Canonicalizer, filter: Boolean = true, useCache: Boolean = false) =
      DomainOntologies(ontologyPath, serializedPath, proc, canonicalizer: Canonicalizer, filter, useCache)
}

object TopoFlowOntology {
  def apply(ontologyPath: String, serializedPath: String, proc: Processor, canonicalizer: Canonicalizer, filter: Boolean = true, useCache: Boolean = false) =
      DomainOntologies(ontologyPath, serializedPath, proc, canonicalizer: Canonicalizer, filter, useCache)
}

object MeshOntology {
  def apply(ontologyPath: String, serializedPath: String, proc: Processor, canonicalizer: Canonicalizer, filter: Boolean = true, useCache: Boolean = false) =
      DomainOntologies(ontologyPath, serializedPath, proc, canonicalizer: Canonicalizer, filter, useCache)
}
