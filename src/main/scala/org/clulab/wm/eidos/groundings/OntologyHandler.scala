package org.clulab.wm.eidos.groundings

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.processors.Processor
import org.clulab.wm.eidos.utils.Canonicalizer


class OntologyHandler(val proc: Processor, val wordToVec: EidosWordToVec, val canonicalizer: Canonicalizer) {

  import OntologyHandler.serializedPath

  def ontologyGroundersFromConfig(config: Config): Seq[EidosOntologyGrounder] = {

    val cacheDir: String = config[String]("cacheDir")
    val useCached: Boolean = config[Boolean]("useCache")
    val selected = config[List[String]]("ontologies")
    for {
      ontologyName <- selected
      path = config[String](ontologyName)
      domainOntology = mkDomainOntology(ontologyName, path, cacheDir, useCached)
    } yield EidosOntologyGrounder(ontologyName, domainOntology, wordToVec)
  }

  // todo: I removed all the variants bc we don't currently need specialization, but we can specialize as needed later
  def mkDomainOntology(name: String, ontologyPath: String, cacheDir: String, useCached: Boolean): DomainOntology = {
    val ontSerializedPath: String = serializedPath(name, cacheDir)
    DomainOntologies(ontologyPath, ontSerializedPath, proc, canonicalizer: Canonicalizer, filter = true, useCache = useCached)
  }

}

object OntologyHandler {

  def serializedPath(name: String, dir: String): String = s"$dir/$name.serialized"

}

