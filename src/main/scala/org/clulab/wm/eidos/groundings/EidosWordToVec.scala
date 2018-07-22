package org.clulab.wm.eidos.groundings

import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.odin.Mention
import org.clulab.wm.eidos.utils.Sourcer
import org.slf4j.LoggerFactory

trait EidosWordToVec {
  type Similarities = Seq[(String, Double)]

  def stringSimilarity(string1: String, string2: String): Double
  def calculateSimilarity(mention1: Mention, mention2: Mention): Double
  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: Seq[ConceptEmbedding], mentionTokenWeights: Map[String, Double]): Similarities
  def makeCompositeVector(t:Iterable[String]): Array[Double]
}

class FakeWordToVec extends EidosWordToVec {

  override def stringSimilarity(string1: String, string2: String): Double =
    throw new RuntimeException("Word2Vec wasn't loaded, please check configurations.")

  def calculateSimilarity(mention1: Mention, mention2: Mention): Double = 0

  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: Seq[ConceptEmbedding], mentionTokenWeights: Map[String, Double]): Similarities = Seq.empty
//  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: ConceptEmbeddings): Seq[(String, Double)] = Seq(("hello", 5.0d))

  def makeCompositeVector(t:Iterable[String]): Array[Double] = Array.emptyDoubleArray
}

class RealWordToVec(var w2v: Word2Vec, topKNodeGroundings: Int) extends EidosWordToVec {

  protected def split(string: String): Array[String] = string.split(" +")

  def stringSimilarity(string1: String, string2: String): Double =
      w2v.avgSimilarity(split(string1), split(string2))

  def calculateSimilarity(mention1: Mention, mention2: Mention): Double = {
    // avgSimilarity does sanitizeWord itself, so it is unnecessary here.
    val sanitisedM1 =  split(mention1.text)
    val sanitisedM2 =  split(mention2.text)
    val similarity = w2v.avgSimilarity(sanitisedM1, sanitisedM2)
    similarity
  }

  // Todo: check this function ...
  def makeCompositeVectorWithInterpolate(word2Vec: Word2Vec, t:Iterable[String], mentionTokenWeights: Map[String, Double]):Array[Double] = {
//    println(s"Composite vector though interpolation ... using token distance ") //.. now using syntax distance ...")
    val mentionToken_wts = t.map { token =>
      if (mentionTokenWeights.contains(token) )
        (token, mentionTokenWeights.get(token).get )
      else
        (token, 0.0)
    }
    word2Vec.interpolate(mentionToken_wts)
  }

  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: Seq[ConceptEmbedding], mentionTokenWeights: Map[String, Double]): Similarities = {
    val sanitizedNameParts = canonicalNameParts.map(Word2Vec.sanitizeWord(_))
    // It could be that the composite vectors below has all zeros even though some values are defined.
    // That wouldn't be OOV, but a real 0 value.  So, conclude OOV only if none is found (all are not found).
    val oov = sanitizedNameParts.forall(w2v.getWordVector(_).isEmpty)

    if (oov)
      Seq.empty
    else {
      // NOTE: replacing makeCompositeVector to interpolate .. to weight the different tokens as determined from (i) synHead-distance + (ii) idf
//      val nodeEmbedding = w2v.makeCompositeVector(sanitizedNameParts)
      val nodeEmbedding = makeCompositeVectorWithInterpolate(w2v, sanitizedNameParts, mentionTokenWeights)
      val similarities = conceptEmbeddings.map { conceptEmbedding =>
        (conceptEmbedding.concept, Word2Vec.dotProduct(conceptEmbedding.embedding, nodeEmbedding))
      }

      similarities.sortBy(-_._2).take(topKNodeGroundings)
    }
  }

  def makeCompositeVector(t: Iterable[String]): Array[Double] = w2v.makeCompositeVector(t)
}

object EidosWordToVec {
  protected val logger = LoggerFactory.getLogger(this.getClass())

  def apply(enabled: Boolean, wordToVecPath: String, topKNodeGroundings: Int): EidosWordToVec = {
    if (enabled) {
      logger.info(s"Loading w2v from ${wordToVecPath}...")
      val source = Sourcer.sourceFromResource(wordToVecPath)

      try {
        val w2v = new Word2Vec(source, None)

        new RealWordToVec(w2v, topKNodeGroundings)
      }
      finally {
        source.close()
      }
    }
    else
      new FakeWordToVec()
  }
}
