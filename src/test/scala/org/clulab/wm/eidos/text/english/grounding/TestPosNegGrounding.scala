package org.clulab.wm.eidos.text.english.grounding

import org.clulab.wm.eidos.groundings.grounders.EidosOntologyGrounder
import org.clulab.wm.eidos.test.EnglishTest

class TestPosNegGrounding extends EnglishTest {
  val ontologyHandler = ieSystem.components.ontologyHandlerOpt.get
  val posNegGrounder = ontologyHandler.ontologyGrounders.find(_.name == "wm_posneg").get.asInstanceOf[EidosOntologyGrounder]
  val conceptEmbeddings = posNegGrounder.conceptEmbeddings
  val word2Vec = ontologyHandler.wordToVec

  {
    // Try out the grounder if that is of interest.
    val text = "Four score and seven years ago our fathers brought forth on this continent a new nation"

    behavior of text

    it should "do something special" in {
      val words = text.split(' ')
      val ontologyGrounding = posNegGrounder.groundStrings(words).head
      val individualGrounding = ontologyGrounding.take(1).head
      val name = individualGrounding.name
      val score = individualGrounding.score
      val negScoreOpt = individualGrounding.negScoreOpt

      name should be ("a special name")
      score should be ("a special score")
      negScoreOpt should be ("a special negScoreOpt")
    }
  }

  {
    // Try out EidosWordToVec if that is more interesting.
    val text = "I have a dream that one day this nation will rise up and live out the true meaning of its creed"

    behavior of text

    it should "do something special" in {
      val words = text.split(' ')
      val similarities = word2Vec.calculateSimilarities(words, conceptEmbeddings)
      val (namer, value) = similarities.head

      namer.name should be ("a special name")
      value should be ("a special value")
    }
  }
}
