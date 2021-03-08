package org.clulab.wm.eidos.groundings

import org.clulab.embeddings.DefaultWordSanitizer
import org.clulab.embeddings.WordEmbeddingMap
import org.clulab.embeddings.WordEmbeddingMapPool
import org.clulab.odin.Mention
import org.clulab.wm.eidoscommon.utils.Logging
import org.clulab.wm.eidoscommon.utils.Namer

trait EidosWordToVec {
  def stringSimilarity(string1: String, string2: String): Float
  def calculateSimilarity(mention1: Mention, mention2: Mention): Float
  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities
  def calculateSimilaritiesWeighted(canonicalNameParts: Array[String], posTags:Seq[String], weight:Float, conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities
  def calculateSimilaritiesWeighted(canonicalNameParts: Array[String], termWeights: Seq[Float], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities

  def makeCompositeVector(t:Iterable[String]): Array[Float]
}

class FakeWordToVec extends EidosWordToVec {

  override def stringSimilarity(string1: String, string2: String): Float =
    throw new RuntimeException("Word2Vec wasn't loaded, please check configurations.")

  def calculateSimilarity(mention1: Mention, mention2: Mention): Float = 0

  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities = Seq.empty
  def calculateSimilaritiesWeighted(canonicalNameParts: Array[String], posTags:Seq[String], weight:Float, conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities = Seq.empty
  def calculateSimilaritiesWeighted(canonicalNameParts: Array[String], termWeights: Seq[Float], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities = Seq.empty

    //  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: ConceptEmbeddings): Seq[(String, Float)] = Seq(("hello", 5.0f))

  def makeCompositeVector(t:Iterable[String]): Array[Float] = Array.emptyFloatArray
}

class RealWordToVec(val w2v: WordEmbeddingMap, topKNodeGroundings: Int) extends EidosWordToVec {

  protected def split(string: String): Array[String] = string.split(" +")

  def stringSimilarity(string1: String, string2: String): Float =
      w2v.avgSimilarity(split(string1), split(string2))

  def calculateSimilarity(mention1: Mention, mention2: Mention): Float = {
    // avgSimilarity does sanitizeWord itself, so it is unnecessary here.
    val sanitisedM1 =  split(mention1.text)
    val sanitisedM2 =  split(mention2.text)
    val similarity = w2v.avgSimilarity(sanitisedM1, sanitisedM2)
    
    similarity
  }

  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities = {
    val sanitizedNameParts = canonicalNameParts.map(EidosWordToVec.sanitizer.sanitizeWord(_))
    // It could be that the composite vectore below has all zeros even though some values are defined.
    // That wouldn't be OOV, but a real 0 value.  So, conclude OOV only if none is found (all are not found).
    val outOfVocabulary = sanitizedNameParts.forall(w2v.isOutOfVocabulary)

    if (outOfVocabulary)
      Seq.empty
    else {
      val nodeEmbedding = w2v.makeCompositeVector(sanitizedNameParts)
      val similarities = conceptEmbeddings.map { conceptEmbedding =>
        (conceptEmbedding.namer, EidosWordToVec.dotProduct(conceptEmbedding.embedding, nodeEmbedding))
      }

      similarities.sortBy(-_._2).take(topKNodeGroundings)
    }
  }

  def calculateSimilaritiesWeighted(canonicalNameParts: Array[String], posTags:Seq[String], nounVerbWeightRatio:Float, conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities = {
    val sanitizedNameParts = canonicalNameParts.map(EidosWordToVec.sanitizer.sanitizeWord(_))
    // It could be that the composite vectore below has all zeros even though some values are defined.
    // That wouldn't be OOV, but a real 0 value.  So, conclude OOV only if none is found (all are not found).
    val outOfVocabulary = sanitizedNameParts.forall(w2v.isOutOfVocabulary)
    val termWeights = posTags.map(tag=>if (tag.startsWith("NN")) nounVerbWeightRatio else 1.0f)

    if (outOfVocabulary)
      Seq.empty
    else {
      val nodeEmbedding = w2v.makeCompositeVectorWeighted(sanitizedNameParts, termWeights)
      val similarities = conceptEmbeddings.map { conceptEmbedding =>
        (conceptEmbedding.namer, EidosWordToVec.dotProduct(conceptEmbedding.embedding, nodeEmbedding))
      }

      similarities.sortBy(-_._2).take(topKNodeGroundings)
    }
  }

  def calculateSimilaritiesWeighted(canonicalNameParts: Array[String], termWeights: Seq[Float], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities = {
    val sanitizedNameParts = canonicalNameParts.map(EidosWordToVec.sanitizer.sanitizeWord(_))
    // It could be that the composite vectore below has all zeros even though some values are defined.
    // That wouldn't be OOV, but a real 0 value.  So, conclude OOV only if none is found (all are not found).
    val outOfVocabulary = sanitizedNameParts.forall(w2v.isOutOfVocabulary)

    if (outOfVocabulary)
      Seq.empty
    else {
      val nodeEmbedding = w2v.makeCompositeVectorWeighted(sanitizedNameParts, termWeights)
      val similarities = conceptEmbeddings.map { conceptEmbedding =>
        (conceptEmbedding.namer, EidosWordToVec.dotProduct(conceptEmbedding.embedding, nodeEmbedding))
      }

      similarities.sortBy(-_._2).take(topKNodeGroundings)
    }
  }

  // This conversion is checked in the unit test for this class.
  def makeCompositeVector(t: Iterable[String]): Array[Float] = {
    val result = w2v.makeCompositeVector(t)
    val result1 = result.toArray
    val result2 = result1.asInstanceOf[Array[Float]]

    result2
  }
}

object EidosWordToVec extends Logging {
  type Similarities = Seq[(Namer, Float)]

  val sanitizer: DefaultWordSanitizer = WordEmbeddingMap.defaultWordSanitizer

  def dotProduct(v1: IndexedSeq[Float], v2: IndexedSeq[Float]): Float = WordEmbeddingMap.dotProduct(v1, v2)

  def makeCachedFilename(path: String, file: String): String =
    path + "/" + file.split('/').last

  def apply(enabled: Boolean, wordToVecPath: String, topKNodeGroundings: Int, cachedPath: String, cached: Boolean = false): EidosWordToVec = {
    if (enabled) {
      logger.info(s"Loading w2v from $wordToVecPath...")

      val w2v =
        if (cached) WordEmbeddingMapPool.getOrElseCreate(cachedPath, compact = true)
        else WordEmbeddingMapPool.getOrElseCreate(wordToVecPath, compact = true)

      new RealWordToVec(w2v, topKNodeGroundings)
    }
    else
      new FakeWordToVec()
  }
}
