package org.clulab.wm.eidos.groundings

import org.clulab.embeddings.DefaultWordSanitizer
import org.clulab.embeddings.WordEmbeddingMap
import org.clulab.embeddings.WordEmbeddingMapPool
import org.clulab.odin.Mention
import org.clulab.wm.eidoscommon.utils.Logging
import org.clulab.wm.eidoscommon.utils.Namer
import org.clulab.wm.eidoscommon.utils.StringUtils

trait EidosWordToVec {
  def stringSimilarity(string1: String, string2: String): Float
  def calculateSimilarity(mention1: Mention, mention2: Mention): Float
  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities
  def calculateSimilaritiesWeighted(canonicalNameParts: Array[String], posTags:Seq[String], weight:Float, conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities
  def calculateSimilaritiesWeighted(canonicalNameParts: Array[String], termWeights: Seq[Float], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities

  def makeCompositeVector(t: Iterable[String]): Array[Float]
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

class RealWordToVec(val w2v: WordEmbeddingMap, topKNodeGroundings: Int,
    groundNegScoreThreshold: Float, groundPenalizeValue: Float) extends EidosWordToVec {

  protected def split(string: String): Array[String] = string.split(" +")

  def stringSimilarity(string1: String, string2: String): Float = {
    val sanitizedString1 = split(string1).map(EidosWordToVec.sanitizer.sanitizeWord)
    val sanitizedString2 = split(string2).map(EidosWordToVec.sanitizer.sanitizeWord)
    val similarity = w2v.avgSimilarity(sanitizedString1, sanitizedString2)

    constrainRange(similarity)
  }

  def calculateSimilarity(mention1: Mention, mention2: Mention): Float =
    stringSimilarity(mention1.text, mention2.text)

  // case class ConceptEmbedding(
  //   val namer: Namer,
  //   embedding: Array[Float],
  //   negEmbeddingOpt: Option[Array[Float]] = None) extends Serializable

  def calculateSimilarities(canonicalNameParts: Array[String], conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities = {
    val sanitizedNameParts = canonicalNameParts.map(EidosWordToVec.sanitizer.sanitizeWord)
    // It could be that the composite vectore below has all zeros even though some values are defined.
    // That wouldn't be OOV, but a real 0 value.  So, conclude OOV only if none is found (all are not found).
    val outOfVocabulary = sanitizedNameParts.forall(w2v.isOutOfVocabulary)

    if (outOfVocabulary)
      Seq.empty
    else {
      val nodeEmbedding = w2v.makeCompositeVector(sanitizedNameParts)
      val similarities = conceptEmbeddings.map { conceptEmbedding =>
        // some smart combination of the similarity to the conceptEmbedding.embedding and ce.negEmbeddingOpt
        (conceptEmbedding.namer, scoreNode(conceptEmbedding.embedding, conceptEmbedding.negEmbeddingOpt, nodeEmbedding))
      }
      similarities.sortBy(-_._2).take(topKNodeGroundings)
    }
  }

  def constrainRange(value: Float): Float = math.max(-1f, math.min(1f, value))

  def scoreNode(posEmbedding: Array[Float], negEmbeddingOpt: Option[Array[Float]], nodeEmbedding: Array[Float]): Float = {
    val posScore = EidosWordToVec.dotProduct(posEmbedding, nodeEmbedding)
    val negScore = negEmbeddingOpt.map(EidosWordToVec.dotProduct(_, nodeEmbedding)).getOrElse(0f)
    val score =
      if (negScore > groundNegScoreThreshold) math.max(-1f, posScore - groundPenalizeValue)
      else posScore

    score
  }

  def calculateSimilaritiesWeighted(canonicalNameParts: Array[String], posTags:Seq[String], nounVerbWeightRatio:Float, conceptEmbeddings: Seq[ConceptEmbedding]): EidosWordToVec.Similarities = {
    val sanitizedNameParts = canonicalNameParts.map(EidosWordToVec.sanitizer.sanitizeWord)
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
    val sanitizedNameParts = canonicalNameParts.map(EidosWordToVec.sanitizer.sanitizeWord)
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

  def makeCompositeVector(t: Iterable[String]): Array[Float] = w2v.makeCompositeVector(t)
}

object EidosWordToVec extends Logging {
  type Similarities = Seq[(Namer, Float)]

  val sanitizer: DefaultWordSanitizer = new DefaultWordSanitizer() // WordEmbeddingMap.defaultWordSanitizer

  def dotProduct(v1: IndexedSeq[Float], v2: IndexedSeq[Float]): Float = WordEmbeddingMap.dotProduct(v1, v2)

  def makeCachedFilename(path: String, file: String): String =
    path + "/" + file.split('/').last

  def apply(enabled: Boolean, wordToVecPath: String, topKNodeGroundings: Int, groundNegScoreThreshold: Float, groundPenalizeValue: Float, cachedPath: String, cached: Boolean = false): EidosWordToVec = {
    if (enabled) {
      logger.info(s"Loading w2v from $wordToVecPath (or possibly $cachedPath)...")

      val name = StringUtils.afterLast(wordToVecPath, '/', all = true, keep = false)
      val resourceLocation = StringUtils.beforeLast(wordToVecPath, '/', all = false, keep = true)
      val fileLocation = cachedPath + "/"
      val w2v = WordEmbeddingMapPool.getOrElseCreate(name, compact = true, fileLocation, resourceLocation)

      new RealWordToVec(w2v, topKNodeGroundings, groundNegScoreThreshold, groundPenalizeValue)
    }
    else
      new FakeWordToVec()
  }
}
