package org.clulab.wm.eidos.document

import com.typesafe.config.Config
import ai.lum.common.ConfigUtils._

import org.clulab.processors.Sentence
import org.clulab.wm.eidos.groundings.{OntologyGrounding, OntologyHandler}
import org.clulab.wm.eidos.groundings.grounders.FlatOntologyGrounder
import org.clulab.wm.eidos.utils.{ Language}

import scala.io.BufferedSource
import scala.io.Source
import java.io.FileNotFoundException
import java.nio.charset.StandardCharsets


class SentenceClassifier(val config:Config, val ontologyHandler: OntologyHandler) {

  // get the classification threshold from eidos.conf file
  val classificationThreshold = config.getString("classificationThreshold").toFloat

  val flatOntologyGrounder = ontologyHandler.ontologyGrounders.filter(_.isInstanceOf[FlatOntologyGrounder]).head.asInstanceOf[FlatOntologyGrounder]
  val conceptEmbeddings = flatOntologyGrounder.asInstanceOf[FlatOntologyGrounder].conceptEmbeddings

  // Load idf weights of the tokens from the resource folder
  val idfWeights = loadTermIDF(config)

  def classify(sentence: Sentence): Float = {
    val sentenceTokens = sentence.words
    val sentenceTokenWeights = sentenceTokens.map{ x => if (idfWeights.contains(x)) idfWeights(x) else 1.0f}

    val groundings = Seq(flatOntologyGrounder.newOntologyGrounding(ontologyHandler.wordToVec.calculateSimilaritiesWeighted(sentenceTokens, sentenceTokenWeights, conceptEmbeddings)))

    // The correlation score of a sentence is set to 0 if it is below the threshold. Change it later if needed.
    if (getTop5Scores(groundings).head>classificationThreshold) getTop5Scores(groundings).head else 0.0f
  }

  // Read idf weights of tokens as a map.
  private def readFromText2Map(filename:String):Map[String, Float] = {

    val outputMap_ = scala.collection.mutable.HashMap[String,Float]()

    val bufferedSource = SentenceClassifier.sourceFromResource(filename)
    for (line <- bufferedSource.getLines) {
      val cols = line.split("_SEP_")
      // do whatever you want with the columns here
      outputMap_(cols(0)) = cols(1).toFloat
    }
    bufferedSource.close

    outputMap_.toMap
  }

  // Load idf scores of the tokens
  private def loadTermIDF(config:Config):Map[String, Float] = {
    // Load resource config:
    val idfWeightsFile = config[String]("tokenIDFWeights")
    readFromText2Map(idfWeightsFile)
  }

  private def getTop5Scores(allGroundings: Seq[OntologyGrounding], grounderName:String ="wm_flattened"): Seq[Float] =
    allGroundings.head
      .take(5)
      .map(_._2)
}

object SentenceClassifier {
  def newFileNotFoundException(path: String): FileNotFoundException = {
    val message1 = path + " (The system cannot find the path specified"
    val message2 = message1 + (if (path.startsWith("~")) ".  Make sure to not use the tilde (~) character in paths in lieu of the home directory." else "")
    val message3 = message2 + ")"

    new FileNotFoundException(message3)
  }

  def sourceFromResource(path: String): BufferedSource = {
    val utf8: String = StandardCharsets.UTF_8.toString

    val url = Option(this.getClass.getResource(path))
      .getOrElse(throw newFileNotFoundException(path))

    Source.fromURL(url, utf8)
  }

  def fromConfig(config: Config, language: String, ontologyHandler: OntologyHandler): Option[SentenceClassifier] = {
    if (language == Language.ENGLISH)
      Some(new SentenceClassifier(config, ontologyHandler))
    else
      None
  }
}

class EidosSentenceClassifier(sentenceClassifierOpt: Option[SentenceClassifier]) {
  val classificationThreshold = sentenceClassifierOpt.get.classificationThreshold

  def classify(sentence: Sentence): Option[Float] = {
    sentenceClassifierOpt.map(_.classify(sentence)).orElse(None)
  }

}
