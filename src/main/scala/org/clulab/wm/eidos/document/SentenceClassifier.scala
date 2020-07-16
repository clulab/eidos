package org.clulab.wm.eidos.document

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.processors.Sentence
import org.clulab.wm.eidos.groundings.FlatOntologyGrounder
import org.clulab.wm.eidos.groundings.OntologyHandler
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.Language
import org.clulab.wm.eidos.utils.Sourcer
import java.io.FileNotFoundException

import org.clulab.wm.eidos.groundings.ConceptEmbedding

class SentenceClassifier(val classificationThreshold: Float, idfWeights: Map[String, Float], ontologyHandler: OntologyHandler) {
  val flatOntologyGrounder: FlatOntologyGrounder = ontologyHandler.ontologyGrounders.collect { case grounder: FlatOntologyGrounder => grounder }.head
  val conceptEmbeddings: Seq[ConceptEmbedding] = flatOntologyGrounder.conceptEmbeddings

  def classify(sentence: Sentence): Float = {
    val words = sentence.words
    val weights = words.map(idfWeights.getOrElse(_, 1.0f))
    val similarities = ontologyHandler.wordToVec.calculateSimilaritiesWeighted(words, weights, conceptEmbeddings)
    val grounding = flatOntologyGrounder.newOntologyGrounding(similarities)
    // The correlation score of a sentence is set to 0 if it is below the threshold. Change it later if needed.
    val classification = grounding.headOption.map(_._2).getOrElse(0.0f)

    classification
  }
}

object SentenceClassifier {
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
    if (language == Language.ENGLISH) {
      val classificationThreshold = config[Double]("classificationThreshold").toFloat
      val idfWeightsFile = config[String]("tokenIDFWeights")
      val idfWeights = readFromText2Map(idfWeightsFile)

      Some(new SentenceClassifier(classificationThreshold, idfWeights, ontologyHandler))
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
