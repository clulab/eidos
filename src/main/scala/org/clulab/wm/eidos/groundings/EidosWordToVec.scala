package org.clulab.wm.eidos.groundings

import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.odin.Mention
import org.clulab.wm.eidos.utils.Sourcer

trait EidosWordToVec {
  type Similarities = Seq[(String, Double)]
  type ConceptEmbeddings = Seq[(String, Array[Double])]

  def stringSimilarity(string1: String, string2: String): Double
  def calculateSimilarity(mention1: Mention, mention2: Mention): Double
  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: ConceptEmbeddings): Similarities
  def makeCompositeVector(t:Iterable[String]): Array[Double]
}

class FakeWordToVec extends EidosWordToVec {

  override def stringSimilarity(string1: String, string2: String): Double =
    throw new RuntimeException("Word2Vec wasn't loaded, please check configurations.")

  def calculateSimilarity(mention1: Mention, mention2: Mention): Double = 0

  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: ConceptEmbeddings): Similarities = Seq.empty
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
  
  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: ConceptEmbeddings): Similarities = {
    val nodeEmbedding = w2v.makeCompositeVector(canonicalNameParts.map(word => Word2Vec.sanitizeWord(word)))
    val oov = nodeEmbedding.forall(_ == 0)

    if (oov)
      Seq.empty
    else {
      val similarities = conceptEmbeddings.map(concept => (concept._1, Word2Vec.dotProduct(concept._2, nodeEmbedding)))

      similarities.sortBy(-_._2).take(topKNodeGroundings)
    }
  }

  def makeCompositeVector(t: Iterable[String]): Array[Double] = w2v.makeCompositeVector(t)
}

object EidosWordToVec {

  def apply(enabled: Boolean, wordToVecPath: String, topKNodeGroundings: Int): EidosWordToVec = {
    if (enabled) {
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
