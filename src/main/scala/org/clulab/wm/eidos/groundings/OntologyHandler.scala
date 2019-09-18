package org.clulab.wm.eidos.groundings

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.wm.eidos.SentencesExtractor
import org.clulab.wm.eidos.groundings.TreeDomainOntology.TreeDomainOntologyBuilder
import org.clulab.wm.eidos.utils.{Canonicalizer, StopwordManager}
import org.slf4j.{Logger, LoggerFactory}


class OntologyHandler(
  val grounders: Seq[EidosOntologyGrounder],
  val wordToVec: EidosWordToVec,
  val sentencesExtractor: SentencesExtractor,
  val canonicalizer: Canonicalizer) {

  def ontologyGrounders: EidosMultiOntologyGrounder = {
    wordToVec match {
      case _: RealWordToVec => new EidosMultiOntologyGrounder(grounders)
      case _: FakeWordToVec => new EidosMultiOntologyGrounder(Seq.empty)
      case _ => ???
    }
  }

  def reground(name: String = "Custom", ontologyYaml: String, canonicalNames: Seq[String], filter: Boolean = true, topk: Int = 10): Array[Array[(String, Float)]] = {
    def reformat(grounding: OntologyGrounding): Array[(String, Float)] ={
      val topGroundings = grounding.take(topk).toArray
      topGroundings.map(gr => (gr._1.name, gr._2))
    }

    //OntologyGrounding
    val ontology = OntologyHandler.mkDomainOntologyFromYaml(name, ontologyYaml, sentencesExtractor, canonicalizer, filter)
    val grounder = EidosOntologyGrounder(name, ontology, wordToVec, canonicalizer)
    val groundings = grounder match {
      case g: EidosOntologyGrounder => canonicalNames.toArray.map(text => g.groundText(text))
      case _ => throw new RuntimeException("Regrounding needs an EidosOntologyGrounder")
    }
    groundings.map(reformat)
  }
}

object OntologyHandler {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def load(config: Config, proc: SentencesExtractor, stopwordManager: StopwordManager): OntologyHandler = {

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
    wordToVec match {
      case _: RealWordToVec =>
        val selected = config[List[String]]("ontologies")
        val enabledOntologies = for {
          ontologyName <- selected
          path = config[String](ontologyName)
          domainOntology = mkDomainOntology(ontologyName, path, proc, canonicalizer, cacheDir, useCached)
        } yield EidosOntologyGrounder(ontologyName, domainOntology, wordToVec, canonicalizer)
        new OntologyHandler(enabledOntologies, wordToVec, proc, canonicalizer)

      case _: FakeWordToVec => new OntologyHandler(Seq.empty, wordToVec, proc, canonicalizer)
      case _ => ???
    }
//    val selected = config[List[String]]("ontologies")
//    val enabledOntologies = for {
//      ontologyName <- selected
//      path = config[String](ontologyName)
//      domainOntology = mkDomainOntology(ontologyName, path, proc, canonicalizer, cacheDir, useCached)
//    } yield EidosOntologyGrounder(ontologyName, domainOntology, wordToVec)
//
//
//    new OntologyHandler(enabledOntologies, wordToVec, proc, canonicalizer)
  }

  // fixme == this all needs to be unified, the constructors in DomainOntolgies and in TreeDomainOntBUilder / here should all
  // be in one place!
  def mkDomainOntology(name: String, ontologyPath: String, sentenceExtractor: SentencesExtractor, canonicalizer: Canonicalizer, cacheDir: String, useCached: Boolean): DomainOntology = {
    val ontSerializedPath: String = serializedPath(name, cacheDir)
    DomainOntologies(ontologyPath, ontSerializedPath, sentenceExtractor, canonicalizer: Canonicalizer, filter = true, useCache = useCached)
  }

  def mkDomainOntologyFromYaml(name: String, ontologyYaml: String, sentenceExtractor: SentencesExtractor, canonicalizer: Canonicalizer, filter: Boolean = true): DomainOntology = {
    new TreeDomainOntologyBuilder(sentenceExtractor, canonicalizer, filter).buildFromYaml(ontologyYaml)
  }

  def serializedPath(name: String, dir: String): String = s"$dir/$name.serialized"
}
