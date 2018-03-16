package org.clulab.wm.eidos.groundings

import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.odin.Mention
import org.clulab.wm.eidos.utils.DomainOntology
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sourcer

trait EidosWordToVec {
  def calculateSimilarity(m1: Mention, m2: Mention): Double
  def calculateSimilarities(canonicalNameParts: Array[String]): Seq[(String, Double)]
}

class RealWordToVec(w2v: Word2Vec, conceptEmbeddings: Map[String, Seq[Double]], topKNodeGroundings: Int) extends EidosWordToVec {

  def calculateSimilarity(m1: Mention, m2: Mention): Double = {
    // avgSimilarity does sanitizeWord itself, so it is unnecessary here.
    val sanitisedM1 =  m1.text.split(" +")
    val sanitisedM2 =  m2.text.split(" +")
    val similarity = w2v.avgSimilarity(sanitisedM1, sanitisedM2)
    
    similarity
  }
  
  def calculateSimilarities(canonicalNameParts: Array[String]): Seq[(String, Double)] = {
    val nodeEmbedding = w2v.makeCompositeVector(canonicalNameParts)
    val similarities = conceptEmbeddings.toSeq.map(concept => (concept._1, Word2Vec.dotProduct(concept._2.toArray, nodeEmbedding)))
    
    similarities.sortBy(- _._2).slice(0, topKNodeGroundings)
  }
}

class FakeWordToVec extends EidosWordToVec {
  
  def calculateSimilarity(m1: Mention, m2: Mention): Double = 0

  def calculateSimilarities(canonicalNameParts: Array[String]): Seq[(String, Double)] = Seq.empty
}

object EidosWordToVec {

  def apply(enabled: Boolean, wordToVecPath: String, domainOntoPath: String, topKNodeGroundings: Int): EidosWordToVec = {
      if (enabled) {
        val ontology = DomainOntology(FileUtils.loadYamlFromResource(domainOntoPath))
        val source = Sourcer.sourceFromResource(wordToVecPath)

        try {
          val w2v = new Word2Vec(source, None)
          val conceptEmbeddings = ontology.iterateOntology(w2v)
  
          new RealWordToVec(w2v, conceptEmbeddings, topKNodeGroundings)
        }
        finally {
          source.close()
        }
      }
      else
        new FakeWordToVec()
  }
}