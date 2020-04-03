package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.utils.Namer
import org.slf4j.{Logger, LoggerFactory}

trait EidosWordToVec {
  // This returns, for compatability reasons, the vector that would traditionally result from
  // makeCompositeVector() if all the words are out of vocabulary (OOV).
  def unknownCompositeVector: Array[Float]

  // This makes vectors for both arrays
  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities

  // This is used to make the vector/array for a single bunch of strings so that they can be compared to another vector.
  def makeCompositeVector(texts: Seq[String]): Option[Array[Float]]
}

class FakeWordToVec extends EidosWordToVec {

  def unknownCompositeVector: Array[Float] = Array.emptyFloatArray

  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities = Seq.empty
//  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: ConceptEmbeddings): Seq[(String, Float)] = Seq(("hello", 5.0f))

  def makeCompositeVector(texts: Seq[String]): Option[Array[Float]] = None
}

class RealWordToVec(val w2v: SanitizedWord2Vec, topKNodeGroundings: Int) extends EidosWordToVec {

  def unknownCompositeVector: Array[Float] = w2v.unknownVector

  def calculateSimilarities(words: Array[String], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities = {
    val nodeEmbeddingOpt = makeCompositeVector(words)

    nodeEmbeddingOpt.map { nodeEmbedding =>
      val similarities = conceptEmbeddings.map { conceptEmbedding =>
        (conceptEmbedding.namer, w2v.dotProduct(conceptEmbedding.embedding, nodeEmbedding))
      }

      similarities.sortBy(-_._2).take(topKNodeGroundings)
    }.getOrElse(Seq.empty)
  }

  def makeCompositeVector(words: Seq[String]): Option[Array[Float]] = w2v.makeCompositeVector(words)
}

object EidosWordToVec {
  type Similarities = Seq[(Namer, Float)]

  protected val builder = new SanitizedWord2VecBuilder()

  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def makeCachedFilename(path: String, file: String): String =
      path + "/" + file.split('/').last + ".serialized"

  def apply(enabled: Boolean, wordToVecPath: String, topKNodeGroundings: Int, cachedPath: String, cached: Boolean = false): EidosWordToVec = {
    if (enabled) {
      logger.info(s"Loading w2v from $wordToVecPath...")

      val w2v =
        if (cached) builder.build(makeCachedFilename(cachedPath, wordToVecPath), resource = false, cached)
        else builder.build(wordToVecPath, resource = true, cached)

      new RealWordToVec(w2v, topKNodeGroundings)
    }
    else
      new FakeWordToVec()
  }
}
