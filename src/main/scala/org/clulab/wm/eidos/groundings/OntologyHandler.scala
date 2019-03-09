package org.clulab.wm.eidos.groundings

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.processors.Processor
import org.clulab.wm.eidos.utils.{Canonicalizer, StopwordManager}
import org.slf4j.{Logger, LoggerFactory}


class OntologyHandler(val wordToVec: EidosWordToVec, val grounders: Seq[EidosOntologyGrounder]) {

  def ontologyGrounders(): MultiOntologyGrounder = {
    wordToVec match {
      case real: RealWordToVec => new MultiOntologyGrounder(grounders)
      case fake: FakeWordToVec => new MultiOntologyGrounder(Seq.empty)
      case _ => ???
    }
  }

}

object OntologyHandler {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def load(config: Config, proc: Processor, stopwordManager: StopwordManager): OntologyHandler = {
    def mkDomainOntology(canonicalizer: Canonicalizer, name: String, ontologyPath: String, cacheDir: String, useCached: Boolean): DomainOntology = {
      val ontSerializedPath: String = serializedPath(name, cacheDir)
      DomainOntologies(ontologyPath, ontSerializedPath, proc, canonicalizer, filter = true, useCache = useCached)
    }

    val canonicalizer = new Canonicalizer(stopwordManager)
    val cacheDir: String = config[String]("cacheDir")
    val useCached: Boolean = config[Boolean]("useCache")

    val wordToVec: EidosWordToVec = {
      // This isn't intended to be (re)loadable.  This only happens once.
      OntologyHandler.logger.info("Loading W2V...")
      EidosWordToVec(
        config[Boolean]("useW2V"),
        config[String]("wordToVecPath"),
        config[Int]("topKNodeGroundings"),
        cacheDir,
        useCached
      )
    }

    // Load enabled ontologies
    val selected = config[List[String]]("ontologies")
    val enabledOntologies = for {
      ontologyName <- selected
      path = config[String](ontologyName)
      domainOntology = mkDomainOntology(canonicalizer, ontologyName, path, cacheDir, useCached)
    } yield EidosOntologyGrounder(ontologyName, domainOntology, wordToVec)

    new OntologyHandler(wordToVec, enabledOntologies)
  }
  def serializedPath(name: String, dir: String): String = s"$dir/$name.serialized"

}

