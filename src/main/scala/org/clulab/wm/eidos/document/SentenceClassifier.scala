package org.clulab.wm.eidos.document

import com.typesafe.config.Config
import org.clulab.processors.Sentence
import org.clulab.wm.eidos.{EidosEnglishProcessor, EidosProcessor, EidosSystem}
import org.clulab.wm.eidos.groundings.FullTreeDomainOntology.FullTreeDomainOntologyBuilder
import org.clulab.wm.eidos.groundings.{EidosWordToVec, FlatOntologyGrounder, OntologyGrounding, OntologyHandler}
import org.clulab.wm.eidos.utils.{Canonicalizer, Language, StopwordManager, StringUtils}

import scala.io.BufferedSource
import scala.io.Source
import java.io
import java.io.FileNotFoundException
import java.nio.charset.StandardCharsets

import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.struct.Interval
import org.clulab.wm.eidos.groundings.OntologyAliases.OntologyGroundings

import scala.collection.mutable.ArrayBuffer
import ai.lum.common.ConfigUtils._



class SentenceClassifier(val config:Config, val ontologyHandler: OntologyHandler) {
  // word2Vec object is also needed to build the embedding for each mention. Word2vec object is contained by ontologyHandler

  // Prepare functions to read from the resource file
  val utf8: String = StandardCharsets.UTF_8.toString

  private def readFromText2Map(filename:String):Map[String, Float] = {

    val outputMap_ = scala.collection.mutable.HashMap[String,Float]()

    val bufferedSource = sourceFromResource(filename)
    for (line <- bufferedSource.getLines) {
      val cols = line.split("_SEP_")
      // do whatever you want with the columns here
      outputMap_(cols(0)) = cols(1).toFloat
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

  private def getTop5Scores(allGroundings: Seq[OntologyGrounding], grounderName:String ="wm_flattened"): Seq[Float] =
    allGroundings.head
      .take(5)
      .map(_._2)

  // Load the full tree ontology， the embeddings are computed for each node, instead of each example.
  // When we use the node to calculate the embedding (but node embedding does not use IDF weight), the best result is
  // using IDF weight to compute sentence embedding, the acc is 0.695, the f1 is 0.778.
  // TODO: this might be changed later

  // TODO: will this consume additional resource? How to make a reference to the components in the ontology handler?
  val flatOntologyGrounder = ontologyHandler.ontologyGrounders.filter(_.isInstanceOf[FlatOntologyGrounder]).head.asInstanceOf[FlatOntologyGrounder]
  val conceptEmbeddings = flatOntologyGrounder.asInstanceOf[FlatOntologyGrounder].conceptEmbeddings
  val idfWeights = loadTermIDF()

  //Tdef classify(sentence: Sentence): Float = {
  def classify(sentence: Sentence): Float = {
    // Maybe we should normalize the sentence tokens . TODO later.
    val sentenceTokens = sentence.words
    val sentenceTokenWeights = sentenceTokens.map{ x => if (idfWeights.contains(x)) idfWeights(x) else 1.0f}

//    println("======")
//    println(sentenceTokens.toSeq)
//    println(sentenceTokenWeights.toSeq)
//    scala.io.StdIn.readLine()

    val groundings = Seq(flatOntologyGrounder.newOntologyGrounding(ontologyHandler.wordToVec.calculateSimilaritiesWeighted(sentenceTokens, sentenceTokenWeights, conceptEmbeddings)))
    getTop5Scores(groundings).head
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
  val utf8: String = StandardCharsets.UTF_8.toString

  // First, load the csv spread sheet
  val sentenceClassifierEvaluationData = readEvaluationData()

  // Second, load the ontology nodes.
  val config = EidosSystem.defaultConfig
  val sentenceExtractor  = EidosProcessor("english", cutoff = 150)
  val tagSet = sentenceExtractor.getTagSet
  val stopwordManager = StopwordManager.fromConfig(config, tagSet)
  val ontologyHandler = OntologyHandler.load(config[Config]("ontologies"), sentenceExtractor, stopwordManager, tagSet)
  val sentenceClassifier = new SentenceClassifier(config, ontologyHandler)

  println("ontology handler loaded")

  // Third, write the actual code to align the concepts
  var correctCount = 0
  for (i <- sentenceClassifierEvaluationData.indices) {
    val sentence = sentenceClassifierEvaluationData(i)._1
    val sentenceObj = sentenceExtractor.annotate(sentence).sentences.head
    val label = sentenceClassifierEvaluationData(i)._2

    val classifierPred = sentenceClassifier.classify(sentenceObj)
    if (classifierPred>0.7 & label==1){
      correctCount+=1
    }
    else if (classifierPred<=0.7 & label==0){
      correctCount+=1
    }
  }
  println(s"accuracy:${correctCount}/${sentenceClassifierEvaluationData.length}")


  def newFileNotFoundException(path: String): FileNotFoundException = {
    val message1 = path + " (The system cannot find the path specified"
    val message2 = message1 + (if (path.startsWith("~")) ".  Make sure to not use the tilde (~) character in paths in lieu of the home directory." else "")
    val message3 = message2 + ")"

    new FileNotFoundException(message3)
  }

  def sourceFromResource(path: String): BufferedSource = {
    val url = Option(this.getClass.getResource(path))
      .getOrElse(throw newFileNotFoundException(path))

    Source.fromURL(url, utf8)
  }

  def readEvaluationData():Seq[(String, Int)] = {
    val sentenceClassifierEvaluationData = ArrayBuffer[(String, Int)]()

    //TODO: read this from resource later
    val spreadsheetPath = "/Users/zhengzhongliang/NLP_Research/2020_WorldModeler/20200703/SentenceClassifierEvaluation.tsv"

    //val bufferedSource = sourceFromResource(spreadsheetPath)
    val bufferedSource = Source.fromFile(spreadsheetPath, utf8)
    for (line <- bufferedSource.getLines) {

      val cols = line.split("\t").map(_.trim)
      // do whatever you want with the columns here
      val sentence = cols(0).toLowerCase()
      val label = cols(1).toInt

      sentenceClassifierEvaluationData.append((sentence, label))
    }

    sentenceClassifierEvaluationData.toSeq
  }

  def getTop5(allGroundings: OntologyGroundings, grounderName:String ="wm_flattened"): Seq[String] =
    allGroundings(grounderName)
      .take(5)
      .map(_._1.name)


}
