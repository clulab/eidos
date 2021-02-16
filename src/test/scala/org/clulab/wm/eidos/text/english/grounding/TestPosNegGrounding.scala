package org.clulab.wm.eidos.text.english.grounding

import org.clulab.wm.eidos.groundings.ConceptEmbedding
import org.clulab.wm.eidos.groundings.EidosWordToVec
import org.clulab.wm.eidos.groundings.OntologyHandler
import org.clulab.wm.eidos.groundings.grounders.EidosOntologyGrounder
import org.clulab.wm.eidos.test.EnglishTest

class TestPosNegGrounding extends EnglishTest {
  val ontologyHandler: OntologyHandler = ieSystem.components.ontologyHandlerOpt.get
  val posNegGrounder: EidosOntologyGrounder = ontologyHandler.ontologyGrounders.find(_.name == "wm_posneg").get.asInstanceOf[EidosOntologyGrounder]
  val conceptEmbeddings: Seq[ConceptEmbedding] = posNegGrounder.conceptEmbeddings
  val word2Vec: EidosWordToVec = ontologyHandler.wordToVec

  behavior of "posNegGrounder"

  it should "do something special" in {
    val text = "cheese"
    val words = text.split(' ')
    val ontologyGrounding = posNegGrounder.groundStrings(words).head
    val individualGrounding = ontologyGrounding.take(1).head
    val name = individualGrounding.name
    val score = individualGrounding.score
    val negScoreOpt = individualGrounding.negScoreOpt
    println("*******************")
    println(name)
    name should be ("wm/concept/causal_factor/health_and_life/nutrition/food_intake")
    //score should be ( 0.5379829)
    //negScoreOpt should be ("a special negScoreOpt")
  }

  behavior of "word2vec"

  it should "do something special" in {
    val text = "cheese"
    val words = text.split(' ')
    val similarities = word2Vec.calculateSimilarities(words, conceptEmbeddings)
    val (namer, value) = similarities.head
    println("---------------------")
    println(namer.name)
    namer.name should be ("wm/concept/causal_factor/health_and_life/nutrition/food_intake")
    //value should be ("a special value")
  }
}
