package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.groundings.CompactWord2Vec
import org.clulab.wm.eidos.groundings.Word2VecUtils
import org.clulab.wm.eidos.utils.Namer
import org.slf4j.{Logger, LoggerFactory}

trait EidosWordToVec {
  // This makes vectors for both arrays
  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities

  // This is used to make the vector/array for a single bunch of strings so that they can be compared to another vector.
  def makeCompositeVector(texts: Iterable[String]): Array[Float]
}

class FakeWordToVec extends EidosWordToVec {

  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities = Seq.empty
//  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: ConceptEmbeddings): Seq[(String, Float)] = Seq(("hello", 5.0f))

  def makeCompositeVector(texts: Iterable[String]): Array[Float] = Array.emptyFloatArray
}

class RealWordToVec(val w2v: CompactWord2Vec, topKNodeGroundings: Int) extends EidosWordToVec {

  protected def split(string: String): Array[String] = string.split(" +")

  protected def dotProduct(v1: Array[Float], v2: Array[Float]): Float = {
    assert(v1.length == v2.length) //should we always assume that v2 is longer? perhaps set shorter to length of longer...
    // This would be way prettier, but it is ~20 times slower
    // v1.indices.foldLeft(0.0f)((sum, i) => sum + v1(i) * v2(i))
    var sum = 0.0f // optimization
    var i = 0 // optimization
    while (i < v1.length) {
      sum += v1(i) * v2(i)
      i += 1
    }
    sum
  }

  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities = {
    val sanitizedNameParts = canonicalNameParts.map(Word2VecUtils.sanitizeWord(_))
    val outOfVocabulary = sanitizedNameParts.forall { sanitizedNamePart =>
      w2v.isOutOfVocabulary(sanitizedNamePart) // && w2v.isOutOfVocabulary(sanitizedNamePart.toLowerCase)
    }

    if (outOfVocabulary)
      Seq.empty
    else {
      val nodeEmbedding = makeCompositeVector(sanitizedNameParts)
      val similarities = conceptEmbeddings.map { conceptEmbedding =>
        (conceptEmbedding.namer, dotProduct(conceptEmbedding.embedding, nodeEmbedding))
      }

      similarities.sortBy(-_._2).take(topKNodeGroundings)
    }
  }

  // These texts are expected to be sanitized in advance.  For instance, their cases should match
  // that used in the vector file.
  def makeCompositeVector(texts: Iterable[String]): Array[Float] = w2v.makeCompositeVector(texts)
}

object EidosWordToVec {
  type Similarities = Seq[(Namer, Float)]

  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def makeCachedFilename(path: String, file: String): String =
      path + "/" + file.split('/').last + ".serialized"

  def apply(enabled: Boolean, wordToVecPath: String, topKNodeGroundings: Int, cachedPath: String, cached: Boolean = false): EidosWordToVec = {
    if (enabled) {
      logger.info(s"Loading w2v from $wordToVecPath...")

      val w2v =
        if (cached) CompactWord2Vec(makeCachedFilename(cachedPath, wordToVecPath), resource = false, cached)
        else CompactWord2Vec(wordToVecPath, resource = true, cached)

      new RealWordToVec(w2v, topKNodeGroundings)
    }
    else
      new FakeWordToVec()
  }
}
