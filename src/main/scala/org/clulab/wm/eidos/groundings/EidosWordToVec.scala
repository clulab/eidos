package org.clulab.wm.eidos.groundings

import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.odin.Mention
import org.clulab.wm.eidos.utils.DomainOntology
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sourcer

class EidosWordToVec(enabled: Boolean, wordToVecPath: String, domainOntoPath: String, topKNodeGroundings: Int) {
  protected val (w2v: Word2Vec, conceptEmbeddings: Map[String, Seq[Double]]) =
      if (enabled) {
        val ontology = DomainOntology(FileUtils.loadYamlFromResource(domainOntoPath))
        val source = Sourcer.sourceFromResource(wordToVecPath)

        try {
          val w2v = new Word2Vec(source, None)
          val conceptEmbeddings = ontology.iterateOntology(w2v)
  
          (w2v, conceptEmbeddings)
        }
        finally {
          source.close()
        }
      }
      else {
        val w2v = new Word2Vec(Map[String, Array[Double]]())
        val conceptEmbeddings = Map[String, Seq[Double]]()
        
        (w2v, conceptEmbeddings)
      }

  def calculateSameAs(m1: Mention, m2: Mention): Double = {
    val sanitisedM1 =  m1.text.split(" +").map(Word2Vec.sanitizeWord(_))
    val sanitisedM2 =  m2.text.split(" +").map(Word2Vec.sanitizeWord(_))
    val score = w2v.avgSimilarity(sanitisedM1, sanitisedM2)
    score
  }
  
  def calculateSimilarities(canonicalNameParts: Array[String]): Seq[(String, Double)] = {
    if (enabled) {
      val nodeEmbedding = w2v.makeCompositeVector(canonicalNameParts)
      // Calc dot prods
      val similarities = conceptEmbeddings.toSeq.map(concept => (concept._1, Word2Vec.dotProduct(concept._2.toArray, nodeEmbedding)))
      
      similarities.sortBy(- _._2).slice(0, topKNodeGroundings)
    }
    else
      Seq.empty
  }
}

object EidosWordToVec {
  def apply(enabled: Boolean, wordToVecPath: String, domainOntoPath: String, topKNodeGroundings: Int) =
      new EidosWordToVec(enabled, wordToVecPath, domainOntoPath, topKNodeGroundings)
}