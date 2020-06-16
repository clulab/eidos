package org.clulab.wm.eidos.document

import com.typesafe.config.Config
import org.clulab.processors.Sentence
import org.clulab.wm.eidos.groundings.FullTreeDomainOntology.FullTreeDomainOntologyBuilder
import org.clulab.wm.eidos.utils.{Canonicalizer, Language}

class SentenceClassifier {

  // Load ontology embeddings and idf
  val a = ontologyHandlerOpt
  val canonicalizer = new Canonicalizer(stopwordManager, tagSet)
  val filter = true
  val ontologyYaml = "wm_compositional"
  val fullTreeDomainOntology = new FullTreeDomainOntologyBuilder(sentenceExtractor, canonicalizer, filter).buildFromYaml(ontologyYaml)

  def classify(sentence: Sentence): Float = {
    for (word <- sentence.words){

    }
    1.0f
  }
}

object SentenceClassifier {

  def fromConfig(config: Config, language: String): Option[SentenceClassifier] = {
    if (language == Language.ENGLISH)
      Some(new SentenceClassifier()) // TODO: Use any config settings necessary
    else
      None
  }
}
