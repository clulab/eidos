package org.clulab.wm.eidos.text.englishGrounding

import org.clulab.wm.eidos.groundings.ConceptEmbedding
import org.clulab.wm.eidos.groundings.EidosWordToVec
import org.clulab.wm.eidos.groundings.IndividualGrounding
import org.clulab.wm.eidos.groundings.grounders.EidosOntologyGrounder
import org.clulab.wm.eidos.test.EnglishGroundingTest

class TestPosNegGrounding extends EnglishGroundingTest {

  val ontologyHandlerOpt = ieSystem.components.ontologyHandlerOpt
  val posNegGrounderOpt = ontologyHandlerOpt
      .flatMap { ontologyHandler =>
        ontologyHandler.ontologyGrounders.find(_.name == "wm_posneg")
      }
      .map(_.asInstanceOf[EidosOntologyGrounder])

  posNegGrounderOpt.foreach { posNegGrounder =>
    // This is not normally the case because grounding is turned off for testing.
    // Turn it on in englishTest.conf.
    val ontologyHandler = ontologyHandlerOpt.get
    val conceptEmbeddings: Seq[ConceptEmbedding] = posNegGrounder.conceptEmbeddings
    val word2Vec: EidosWordToVec = ontologyHandler.wordToVec

    behavior of "posNegGrounder"

    it should "read nodes with negative scores" in {
      val hasNegativeScores = posNegGrounder.conceptEmbeddings.exists { conceptEmbedding =>
        conceptEmbedding.negEmbeddingOpt.isDefined
      }

      hasNegativeScores should be(true)
    }

    it should "read nodes without negative scores" in {
      val hasNotNegativeScores = posNegGrounder.conceptEmbeddings.exists { conceptEmbedding =>
        conceptEmbedding.negEmbeddingOpt.isEmpty
      }

      hasNotNegativeScores should be(true)
    }

    it should "simply ground somewhere" in {
      val text = "cheese"
      val words = text.split(' ')
      val ontologyGrounding = posNegGrounder.groundStrings(words).head
      val individualGroundingOpt = ontologyGrounding.headOption

      individualGroundingOpt should be(defined)
    }

    val text = "yield harvest harvesting cultivate groundnut production production fruits"
    val words = text.split(' ')
    val negNodeName = "wm/concept/causal_factor/agriculture/neg_crop_production"
    val posNodeName = "wm/concept/causal_factor/agriculture/pos_crop_production"

    it should "ground to the right node with positive examples" in {
      val ontologyGrounding = posNegGrounder.groundStrings(words).head
      val individualGrounding: IndividualGrounding = ontologyGrounding.take(1).head
      val name = individualGrounding.name

      name should be(posNodeName)
    }

    behavior of "word2vec"

    it should "change grounding when negative examples are added" in {
      val negConceptEmbedding = conceptEmbeddings.find { conceptEmbedding =>
        conceptEmbedding.namer.name == negNodeName
      }.get
      val posConceptEmbedding = negConceptEmbedding.copy(negEmbeddingOpt = None)

      val negSimilarity = word2Vec.calculateSimilarities(words, Seq(negConceptEmbedding)).head._2
      val posSimilarity = word2Vec.calculateSimilarities(words, Seq(posConceptEmbedding)).head._2

      posSimilarity should be > negSimilarity
    }
  }
}
