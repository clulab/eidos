package org.clulab.wm.eidos.groundings

import org.clulab.embeddings.word2vec.CompactWord2Vec

class SanitizedWord2Vec(buildType: SanitizedWord2VecBuilder.BuildType, sanitizer: Sanitizer) extends CompactWord2Vec(buildType){
  protected val unknownRowOpt: Option[Int] = map.get("")

  protected val (sanitizedMap, unsanitizedMap) = {
    val groupedMaps = map.groupBy[Boolean] { case (key, value) =>
      key.startsWith("\t") // Sanitized words begin with tab
    }
    val sanitizedMap = groupedMaps.get(true).map { map =>
      map.map { case (key, value) =>
        key.substring(1) -> value // Remove the tab.
      }
    }.getOrElse(SanitizedWord2VecBuilder.emptyMap)
    val unsanitizedMap = groupedMaps.getOrElse(false, SanitizedWord2VecBuilder.emptyMap)

    map.clear // It is no longer necessary to hold the original map.
    (sanitizedMap, unsanitizedMap)
  }

  protected val cachedUnknownVector: SanitizedWord2VecBuilder.ArrayType = {
    val result = new SanitizedWord2VecBuilder.ArrayType(columns)

    if (unknownRowOpt.isDefined)
      add(result, unknownRowOpt.get)
    result
  }

  def unknownVector: SanitizedWord2VecBuilder.ArrayType = cachedUnknownVector

  protected def getRow(word: String): Option[Int] = {
    // Prefer the word when possible.
    val bestRowOpt = unsanitizedMap.get(word)
    // Make do with the sanitized word if not.
    val betterRowOpt = bestRowOpt.orElse {
      val sanitizedWordOpt = sanitizer.sanitize(word)

      // Keep in mind that sanitization may erase the entire word
      // and tht the sanitized map may not contain the word anyway.
      sanitizedWordOpt.flatMap(sanitizedMap.get(_))
    }

    betterRowOpt
  }

  protected def add(dest: SanitizedWord2VecBuilder.ArrayType, freq: Int, srcRow: Int): Unit = {
    val srcOffset = srcRow * columns
    var i = 0 // optimization

    while (i < columns) {
      dest(i) += array(srcOffset + i) * freq
      i += 1
    }
  }

  def dotProduct(v1: Array[Float], v2: Array[Float]): Float = {
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

  // This has recently been changed to return an Option.  The intention is that if the entire
  // collection of words is OOV (out of vocabulary), then None is returned and the caller can
  // deal with it, probably by not finding any grounding at all.  The intention is to avoid
  // whatever grounding happens to match the vectors for unknown words.  If any grounding should
  // be associated with that at all, it will be up to the caller to find it.  Returning None
  // also does away with immediate need for the previously used isOutOfVocabulary() method.
  def makeCompositeVector(words: Seq[String]): Option[SanitizedWord2VecBuilder.ArrayType] = {
    val rows = words.flatMap { word => getRow(word) }

    if (rows.nonEmpty) {
      val total = new SanitizedWord2VecBuilder.ArrayType(columns) // Start with this.
      val count = // This is how many vectors will be added.
        if (unknownRowOpt.isDefined) rows.size
        else words.size

      rows.foreach(add(total, _))
      if (unknownRowOpt.isDefined) // Account for misses.
      add(total, count - rows.size, unknownRowOpt.get)
      if (count > 1) // Only normalize when required.
      norm(total)
      Some(total)
    }
    else
      None // It is completely OOV!
  }
}
