package org.clulab.wm.eidos.document

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigValueFactory
import org.clulab.processors.Sentence
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.{ConceptEmbedding, OntologyHandler, SingleOntologyNodeGrounding}
import org.clulab.wm.eidos.groundings.grounders.FlatOntologyGrounder
import org.clulab.wm.eidoscommon.Language
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.Sourcer

import scala.collection.JavaConverters._

import java.io.FileNotFoundException

class SentenceClassifier(val classificationThreshold: Float, idfWeights: Map[String, Float], ontologyHandler: OntologyHandler, flatOntologyGrounder: FlatOntologyGrounder) {
  val conceptEmbeddings: Seq[ConceptEmbedding] = flatOntologyGrounder.conceptEmbeddings

  def classify(sentence: Sentence): Float = {
    val words = sentence.words
    val weights = words.map(idfWeights.getOrElse(_, 1.0f))
    val similarities = ontologyHandler.wordToVec.calculateSimilaritiesWeighted(words, weights, conceptEmbeddings)
    // FIXME: assumes flat grounding
    val grounding = flatOntologyGrounder.newOntologyGrounding(similarities.map(SingleOntologyNodeGrounding(_)))
    // The correlation score of a sentence is set to 0 if it is below the threshold. Change it later if needed.
    val classificationRaw = grounding.headOption.map(_.score).getOrElse(0.0f)

    if (classificationRaw>classificationThreshold) {classificationRaw} else 0.0f
  }
}

object SentenceClassifier {

  def enable(config: Config = EidosSystem.defaultConfig): Config = {
    // An enabled SentenceClassifier does need to have a flat ontology available.
    // It will be made the only ontology with this code, so be careful.
    config
      .withValue(
        "sentenceClassifier.enable",
        ConfigValueFactory.fromAnyRef(true)
      )
      .withValue(
        "ontologies.ontologies",
        ConfigValueFactory.fromIterable(Iterable("wm_flattened").asJava)
      )
  }

  def newFileNotFoundException(path: String): FileNotFoundException = {
    val innerMessage =
      if (path.startsWith("~")) ".  Make sure to not use the tilde (~) character in paths in lieu of the home directory."
      else ""
    val message = s"$path (The system cannot find the path specified$innerMessage)"

    new FileNotFoundException(message)
  }

  // Read idf weights of tokens as a map.
  private def readFromText2Map(filename: String): Map[String, Float] = {
    Sourcer.sourceFromResource(filename).autoClose { bufferedSource =>
      bufferedSource.getLines.map { line =>
        val cols = line.split('\t')
        // do whatever you want with the columns here
        cols(0) -> cols(1).toFloat
      }.toMap
    }
  }

  def fromConfig(config: Config, language: String, ontologyHandler: OntologyHandler): Option[SentenceClassifier] = {
    // make a way to disable in the config
    if (!config.apply[Boolean]("enable")) return None

    val flatOntologyGrounders = ontologyHandler.ontologyGrounders.collect { case grounder: FlatOntologyGrounder => grounder }

    if (language == Language.ENGLISH && flatOntologyGrounders.nonEmpty) {
      val classificationThreshold = config.apply[Double]("classificationThreshold").toFloat
      val idfWeightsFile = config.apply[String]("tokenIDFWeights")
      val idfWeights = readFromText2Map(idfWeightsFile)

      Some(new SentenceClassifier(classificationThreshold, idfWeights, ontologyHandler, flatOntologyGrounders.head))
    }
    else
      None
  }
}

class EidosSentenceClassifier(sentenceClassifierOpt: Option[SentenceClassifier]) {
  val classificationThreshold: Float = sentenceClassifierOpt.map(_.classificationThreshold).getOrElse(0.0f)

  def classify(sentence: Sentence): Option[Float] = {
    sentenceClassifierOpt.map(_.classify(sentence)).orElse(None)
  }
}
