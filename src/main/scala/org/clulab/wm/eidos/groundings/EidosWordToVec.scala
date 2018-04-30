package org.clulab.wm.eidos.groundings

import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.odin.Mention
import org.clulab.wm.eidos.utils.Sourcer

class EidosWordToVec(var w2v: Word2Vec, topKNodeGroundings: Int) {

  protected def split(string: String): Array[String] = string.split(" +")

  def stringSimilarity(s1: String, s2: String): Double =
      w2v.avgSimilarity(split(s1), split(s2))

  def calculateSimilarity(m1: Mention, m2: Mention): Double = {
    // avgSimilarity does sanitizeWord itself, so it is unnecessary here.
    val sanitisedM1 =  split(m1.text)
    val sanitisedM2 =  split(m2.text)
    val similarity = w2v.avgSimilarity(sanitisedM1, sanitisedM2)
    
    similarity
  }
  
  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: Map[String, Seq[Double]]): Seq[(String, Double)] = {
    val nodeEmbedding = w2v.makeCompositeVector(canonicalNameParts.map(word => Word2Vec.sanitizeWord(word)))
    val similarities = conceptEmbeddings.toSeq.map(concept => (concept._1, Word2Vec.dotProduct(concept._2.toArray, nodeEmbedding)))
    
    similarities.sortBy(- _._2).slice(0, topKNodeGroundings)
  }
}

object EidosWordToVec {

  def apply(wordToVecPath: String, topKNodeGroundings: Int): EidosWordToVec = {
    val source = Sourcer.sourceFromResource(wordToVecPath)

    try {
      val w2v = new Word2Vec(source, None)

      new EidosWordToVec(w2v, topKNodeGroundings)
    }
    finally {
      source.close()
    }
  }
}