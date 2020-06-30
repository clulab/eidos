package org.clulab.wm.eidos.document

import com.typesafe.config.Config
import org.clulab.processors.Sentence
import org.clulab.wm.eidos.{EidosEnglishProcessor, EidosSystem}
import org.clulab.wm.eidos.groundings.FullTreeDomainOntology.FullTreeDomainOntologyBuilder
import org.clulab.wm.eidos.groundings.{FlatOntologyGrounder, OntologyHandler}
import org.clulab.wm.eidos.utils.{Canonicalizer, Language, StringUtils}
import scala.io.BufferedSource
import scala.io.Source
import java.io
import java.io.FileNotFoundException
import java.nio.charset.StandardCharsets


class SentenceClassifier(val config:Config, val ontologyHandler: OntologyHandler) {

  // Prepare functions to read from the resource file
  val utf8: String = StandardCharsets.UTF_8.toString

  private def readFromText2Map(filename:String):Map[String, Float] = {

    val outputMap_ = scala.collection.mutable.HashMap[String,Float]()

    val bufferedSource = sourceFromResource(filename)
    for (line <- bufferedSource.getLines) {
      val cols = line.split("_SEP_")
      // do whatever you want with the columns here
      outputMap_(cols(0)) = cols(1).toInt
    }
    bufferedSource.close

    outputMap_.toMap
  }

  private def newFileNotFoundException(path: String): FileNotFoundException = {
    val message1 = path + " (The system cannot find the path specified"
    val message2 = message1 + (if (path.startsWith("~")) ".  Make sure to not use the tilde (~) character in paths in lieu of the home directory." else "")
    val message3 = message2 + ")"

    new FileNotFoundException(message3)
  }

  private def sourceFromResource(path: String): BufferedSource = {
    val url = Option(this.getClass.getResource(path))
      .getOrElse(throw newFileNotFoundException(path))

    Source.fromURL(url, utf8)
  }


  // Load idf scores of the tokens
  private def loadTermIDF():Map[String, Float] = {
    // Load resource config:
    val idfWeightsFile = config.getString("sentenceClassifier.tokenIDFWeights")
    readFromText2Map(idfWeightsFile)
  }

  // Load the full tree ontology， the embeddings are computed for each node, instead of each example.
  // When we use the node to calculate the embedding (but node embedding does not use IDF weight), the best result is
  // using IDF weight to compute sentence embedding, the acc is 0.695, the f1 is 0.778.
  // TODO: this might be changed later

  val flatOntologyGrounder = ontologyHandler.ontologyGrounders.filter(_.isInstanceOf[FlatOntologyGrounder]).head
  val conceptEmbeddings = flatOntologyGrounder.asInstanceOf[FlatOntologyGrounder].conceptEmbeddings
  //val idfWeights = loadTermIDF()

  def classify(sentence: Sentence): Float = {
    // Maybe we should normalize the sentence tokens . TODO later.
    for (word <- sentence.words){

    }
    1.0f
  }
}

object SentenceClassifier {

  def fromConfig(config: Config, language: String, ontologyHandler: OntologyHandler): Option[SentenceClassifier] = {
    if (language == Language.ENGLISH)
      Some(new SentenceClassifier(config, ontologyHandler)) // TODO: Use any config settings necessary
    else
      None
  }
}

object DebugSentenceClassifier extends App{

  val config = EidosSystem.defaultConfig
  val eidosSystem = new EidosSystem(config)

  //
  if (eidosSystem.components.proc.isInstanceOf[EidosEnglishProcessor]){
    val sentenceClassifier = eidosSystem.components.proc.asInstanceOf[EidosEnglishProcessor].sentenceClassifierOpt

    println("successfully loaded sentence classifier from eidos")
    //println(sentenceClassifier.get.idfWeights)

  }

}
