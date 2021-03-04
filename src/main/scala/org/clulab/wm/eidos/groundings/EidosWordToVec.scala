package org.clulab.wm.eidos.groundings

import java.io.File
import org.clulab.embeddings.{CompactWordEmbeddingMap => CompactWord2Vec}
import org.clulab.embeddings.{WordEmbeddingMap => Word2Vec}
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

class RealWordToVec(val w2v: CompactWord2Vec, topKNodeGroundings: Int) extends EidosWordToVec {

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

  def dotProduct(v1: Array[Float], v2: Array[Float]): Float = {
    assert(v1.length == v2.length) //should we always assume that v2 is longer? perhaps set shorter to length of longer...
    // This would be way prettier, but it is ~20 times slower
    // v1.indices.foldRight(0.0f)((i, sum) => sum + v1(i) * v2(i))
    var sum = 0.0f // optimization
    var i = 0 // optimization
    while (i < v1.length) {
      sum += v1(i) * v2(i)
      i += 1
    }
    sum
  }

  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities = {
    val sanitizedNameParts = canonicalNameParts.map(Word2Vec.sanitizeWord(_))
    // It could be that the composite vectore below has all zeros even though some values are defined.
    // That wouldn't be OOV, but a real 0 value.  So, conclude OOV only if none is found (all are not found).
    val outOfVocabulary = sanitizedNameParts.forall(w2v.isOutOfVocabulary(_))

    if (outOfVocabulary)
      Seq.empty
    else {
      val nodeEmbedding = w2v.makeCompositeVector(sanitizedNameParts)
      val similarities = conceptEmbeddings.map { conceptEmbedding =>
        (conceptEmbedding.namer, dotProduct(conceptEmbedding.embedding, nodeEmbedding))
      }

      similarities.sortBy(-_._2).take(topKNodeGroundings)
    }
  }

  def calculateSimilaritiesWeighted(canonicalNameParts: Array[String], posTags:Seq[String], nounVerbWeightRatio:Float, conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities = {
    val sanitizedNameParts = canonicalNameParts.map(Word2Vec.sanitizeWord(_))
    // It could be that the composite vectore below has all zeros even though some values are defined.
    // That wouldn't be OOV, but a real 0 value.  So, conclude OOV only if none is found (all are not found).
    val outOfVocabulary = sanitizedNameParts.forall(w2v.isOutOfVocabulary(_))
    val termWeights = posTags.map(tag=>if (tag.startsWith("NN")) nounVerbWeightRatio else 1.0f)

    if (outOfVocabulary)
      Seq.empty
    else {
      val nodeEmbedding = w2v.makeCompositeVectorWeighted(sanitizedNameParts, termWeights)
      val similarities = conceptEmbeddings.map { conceptEmbedding =>
        (conceptEmbedding.namer, dotProduct(conceptEmbedding.embedding, nodeEmbedding))
      }

      similarities.sortBy(-_._2).take(topKNodeGroundings)
    }
  }

  def calculateSimilaritiesWeighted(canonicalNameParts: Array[String], termWeights: Seq[Float], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities = {
    val sanitizedNameParts = canonicalNameParts.map(Word2Vec.sanitizeWord(_))
    // It could be that the composite vectore below has all zeros even though some values are defined.
    // That wouldn't be OOV, but a real 0 value.  So, conclude OOV only if none is found (all are not found).
    val outOfVocabulary = sanitizedNameParts.forall(w2v.isOutOfVocabulary(_))

    if (outOfVocabulary)
      Seq.empty
    else {
      val nodeEmbedding = w2v.makeCompositeVectorWeighted(sanitizedNameParts, termWeights)
      val similarities = conceptEmbeddings.map { conceptEmbedding =>
        (conceptEmbedding.namer, dotProduct(conceptEmbedding.embedding, nodeEmbedding))
      }

      similarities.sortBy(-_._2).take(topKNodeGroundings)
    }
  }

  def makeCompositeVector(t: Iterable[String]): Array[Float] = w2v.makeCompositeVector(t)
}

object EidosWordToVec extends Logging {
  type Similarities = Seq[(Namer, Float)]

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
