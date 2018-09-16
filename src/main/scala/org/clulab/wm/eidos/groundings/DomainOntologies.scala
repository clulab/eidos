package org.clulab.wm.eidos.groundings

import org.clulab.processors.Processor
import org.clulab.wm.eidos.groundings.CompactDomainOntology.CompactDomainOntologyBuilder
import org.clulab.wm.eidos.groundings.TreeDomainOntology.TreeDomainOntologyBuilder
import org.slf4j.LoggerFactory

object DomainOntologies {
  val logger = LoggerFactory.getLogger(this.getClass())

  def serializedPath(name: String, dir: String): String = s"$dir/$name.serialized"

  def apply(name: String, ontologyPath: String, cachedDir: String, proc: Processor, filter: Boolean = true, loadSerialized: Boolean = false): DomainOntology = {
    if (loadSerialized)
      CompactDomainOntology.load(serializedPath(name, cachedDir))
    else {
      logger.info("Processing yml ontology...")
      val tree = new TreeDomainOntologyBuilder(name, ontologyPath, cachedDir, proc, filter).build()
      // This is how it might be done in the compactor
      val compact = new CompactDomainOntologyBuilder(tree).build()
      tree
    }
  }
}

// These are just here for when behavior might have to start differing.
object UNOntology {
  def apply(name: String, ontologyPath: String, cacheDir: String, proc: Processor, filter: Boolean = true, loadSerialized: Boolean = false) =
      DomainOntologies(name, ontologyPath, cacheDir, proc, filter, loadSerialized)
}

object WDIOntology {
  def apply(name: String, ontologyPath: String, cacheDir: String, proc: Processor, filter: Boolean = true, loadSerialized: Boolean = false) =
      DomainOntologies(name, ontologyPath, cacheDir, proc, filter, loadSerialized)
}

object FAOOntology {
  def apply(name: String, ontologyPath: String, cacheDir: String, proc: Processor, filter: Boolean = true, loadSerialized: Boolean = false) =
      DomainOntologies(name, ontologyPath, cacheDir, proc, filter, loadSerialized)
}

object TopoFlowOntology {
  def apply(name: String, ontologyPath: String, cacheDir: String, proc: Processor, filter: Boolean = true, loadSerialized: Boolean = false) =
      DomainOntologies(name, ontologyPath, cacheDir, proc, filter, loadSerialized)
}

object MeshOntology {
  def apply(name: String, ontologyPath: String, cachedDir: String, proc: Processor, filter: Boolean = true, loadSerialized: Boolean = false) =
      DomainOntologies(name, ontologyPath, cachedDir, proc, filter, loadSerialized)
}
