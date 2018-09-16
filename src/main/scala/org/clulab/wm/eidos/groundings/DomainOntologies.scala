package org.clulab.wm.eidos.groundings

import org.clulab.processors.Processor
import org.clulab.wm.eidos.groundings.CompactDomainOntology.CompactDomainOntologyBuilder
import org.clulab.wm.eidos.groundings.TreeDomainOntology.TreeDomainOntologyBuilder
import org.slf4j.LoggerFactory

object DomainOntologies {
  val logger = LoggerFactory.getLogger(this.getClass())

  def serializedPath(name: String, dir: String): String = s"$dir/$name.serialized"

  def apply(ontologyPath: String, serializedPath: String, proc: Processor, filter: Boolean = true, loadSerialized: Boolean = false): DomainOntology = {
    if (loadSerialized)
      CompactDomainOntology.load(serializedPath)
    else {
      logger.info("Processing yml ontology...")
      new TreeDomainOntologyBuilder(ontologyPath, proc, filter).build()
    }
  }
}

// These are just here for when behavior might have to start differing.
object UNOntology {
  def apply(ontologyPath: String, serializedPath: String, proc: Processor, filter: Boolean = true, loadSerialized: Boolean = false) =
      DomainOntologies(ontologyPath, serializedPath, proc, filter, loadSerialized)
}

object WDIOntology {
  def apply(ontologyPath: String, serializedPath: String, proc: Processor, filter: Boolean = true, loadSerialized: Boolean = false) =
      DomainOntologies(ontologyPath, serializedPath, proc, filter, loadSerialized)
}

object FAOOntology {
  def apply(ontologyPath: String, serializedPath: String, proc: Processor, filter: Boolean = true, loadSerialized: Boolean = false) =
      DomainOntologies(ontologyPath, serializedPath, proc, filter, loadSerialized)
}

object TopoFlowOntology {
  def apply(ontologyPath: String, serializedPath: String, proc: Processor, filter: Boolean = true, loadSerialized: Boolean = false) =
      DomainOntologies(ontologyPath, serializedPath, proc, filter, loadSerialized)
}

object MeshOntology {
  def apply(ontologyPath: String, serializedPath: String, proc: Processor, filter: Boolean = true, loadSerialized: Boolean = false) =
      DomainOntologies(ontologyPath, serializedPath, proc, filter, loadSerialized)
}
