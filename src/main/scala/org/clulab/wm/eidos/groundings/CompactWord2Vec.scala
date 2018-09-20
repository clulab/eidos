package org.clulab.wm.eidos.groundings

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import org.clulab.embeddings.word2vec.Word2Vec
import org.clulab.utils.ClassLoaderObjectInputStream
import org.clulab.wm.eidos.utils.Sourcer
import org.slf4j.LoggerFactory

//import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class CompactWord2Vec(buildType: CompactWord2Vec.BuildType) {
  protected val map: CompactWord2Vec.MapType = buildType._1
  protected val array: CompactWord2Vec.ArrayType = buildType._2
  protected val dimension: Int = array.size / map.size

  def get(word: String): Option[CompactWord2Vec.ArrayType] = {
    if (map.contains(word)) {
      val offset = map(word) * dimension
      val vec = new CompactWord2Vec.ArrayType(dimension)

      for (i <- 0 until dimension)
        vec(i) = array(offset + i)
      Some(vec)
    }
    else None
  }

  def keys = map.keys

  def save(filename: String): Unit = {
    val words = map.toArray.sortBy(_._2).map(_._1).mkString("\n")
    val objectOutputStream = new ObjectOutputStream(new FileOutputStream(filename))
    // Writing is performed in two steps so that the parts can be processed separately
    // when read back in.
    objectOutputStream.writeObject(words)
    objectOutputStream.writeObject(array)
    objectOutputStream.close()
  }

  def dotProduct(row1: Int, row2: Int): Float = {
    val offset1 = row1 * dimension
    val offset2 = row2 * dimension
    var sum = 0.0f

    for (i <- 0 until dimension)
      sum += array(offset1 + i) * array(offset2 + i)
    sum
  }

  /**
    * Computes the similarity between two given words
    * IMPORTANT: words here must already be normalized using Word2VecUtils.sanitizeWord()!
    * @param w1 The first word
    * @param w2 The second word
    * @return The cosine similarity of the two corresponding vectors
    */
  def similarity(w1: String, w2: String): Float = {
    val v1o = map.get(w1)
    if (v1o.isEmpty) return -1
    val v2o = map.get(w2)
    if (v2o.isEmpty) return -1

    dotProduct(v1o.get, v2o.get)
  }

  /** Adds the content of src to dest, in place */
  protected def add(dest: Int, src: Int): Unit = {
    val destOffset = dest * dimension
    val srcOffset = src * dimension

    for (i <- 0 until dimension)
      array(destOffset + i) += array(srcOffset + i)
  }

  protected def add(dest: Array[Float], src: Int): Unit = {
    val srcOffset = src * dimension

    for (i <- 0 until dimension)
      dest(i) += array(srcOffset + i)
  }

  def isOutOfVocabulary(word: String): Boolean = !map.contains(Word2Vec.sanitizeWord(word))

  /** Normalizes this vector to length 1, in place */
  def norm(weights: Array[Float]): Array[Float] = {
    var i = 0
    var len = 0.0f

    while (i < weights.length) {
      len += weights(i) * weights(i)
      i += 1
    }
    len = math.sqrt(len).toFloat

    i = 0
    if (len != 0) {
      while (i < weights.length) {
        weights(i) /= len
        i += 1
      }
    }
    weights
  }

  def makeCompositeVector(t: Iterable[String]): Array[Float] = {
    val vTotal = new Array[Float](dimension)

    for (s <- t) {
      val v = map.get(s)
      if (v.isDefined)
        add(vTotal, v.get)
    }
    norm(vTotal)
  }

  /**
    * Finds the average word2vec similarity between any two words in these two texts
    * IMPORTANT: words here must be words not lemmas!
    */
  def avgSimilarity(t1: Iterable[String], t2: Iterable[String]): Float = {
    val st1 = t1.map(Word2Vec.sanitizeWord(_))
    val st2 = t2.map(Word2Vec.sanitizeWord(_))
    val (score, pairs) = sanitizedAvgSimilarity(st1, st2)

    score
  }

  /**
    * Finds the average word2vec similarity between any two words in these two texts
    * IMPORTANT: words here must already be normalized using Word2VecUtils.sanitizeWord()!
    * Changelog: (Peter/June 4/2014) Now returns words list of pairwise scores, for optional answer justification.
    */
  protected def sanitizedAvgSimilarity(t1: Iterable[String], t2: Iterable[String]): (Float, ArrayBuffer[(Float, String, String)]) = {
    // Top words
    val pairs = new ArrayBuffer[(Float, String, String)]

    var avg = 0.0f
    var count = 0
    for (s1 <- t1) {
      val v1 = map.get(s1)
      if (v1.isDefined) {
        for (s2 <- t2) {
          val v2 = map.get(s2)
          if (v2.isDefined) {
            val s = dotProduct(v1.get, v2.get)
            avg += s
            count += 1

            // Top Words
            pairs.append((s, s1, s2))
          }
        }
      }
    }
    if (count != 0) (avg / count, pairs)
    else (0, pairs)
  }
}

object CompactWord2Vec {
  protected type MutableMapType = mutable.HashMap[String, Int]
//  protected type MutableMapType = java.util.HashMap[String, Int]

  type MapType = MutableMapType // Skip conversion to immutable.
  type ArrayType = Array[Float]

  protected type BuildType = (MutableMapType, ArrayType)

  protected type StoreType = (String, ArrayType)

  protected val logger = LoggerFactory.getLogger(classOf[CompactWord2Vec])

  def apply(filename: String, resource: Boolean = true, cached: Boolean = false): CompactWord2Vec = {
    logger.debug("Started to load word2vec matrix from file " + filename + "...")
    val buildType =
        if (cached) loadBin(filename)
        else loadTxt(filename, resource)
    logger.debug("Completed matrix loading.")
    new CompactWord2Vec(buildType)
  }

  protected def loadTxt(filename: String, resource: Boolean): BuildType = {
    val source =
        if (resource) Sourcer.sourceFromResource(filename)
        else Sourcer.sourceFromFile(filename)
    val lines = source.getLines()
    val build = buildMatrix(lines)
    source.close()
    build
  }

  def updatedLoad[A](filename: String, classProvider: Any = this): A = {
    val classLoader = classProvider.getClass().getClassLoader()
    val fileInputStream = new FileInputStream(filename)
    var objectInputStream: ObjectInputStream = null

    try {
      objectInputStream = new ClassLoaderObjectInputStream(classLoader, fileInputStream)

      objectInputStream.readObject().asInstanceOf[A]
    }
    finally {
      if (objectInputStream != null)
        objectInputStream.close()
      else
        fileInputStream.close()
    }
  }

  protected def loadBin(filename: String): BuildType = {
    // This is the original code
//    val (text, array) = updatedLoad[StoreType](filename, this)
//    val words = text.split('\n')
//    val map: MapType = words.zipWithIndex.toMap.asInstanceOf[MapType]
//    (map, array)

    // This is "unrolled" for performance purposes.
    val objectInputStream = new ClassLoaderObjectInputStream(this.getClass().getClassLoader(), new FileInputStream(filename))
    val map: MapType = new MutableMapType()

    {
      // This is so that text can be abandoned at the end of the block, before the array is read.
      val text = objectInputStream.readObject().asInstanceOf[String]
      val stringBuilder = new StringBuilder
      var count = 0

      for (i <- 0 until text.size) {
        val c = text(i)

        if (c == '\n') {
          map += ((stringBuilder.result(), count))
          count += 1
          stringBuilder.clear()
        }
        else
          stringBuilder.append(c)
      }
      map += ((stringBuilder.result(), count))
    }

    val array = objectInputStream.readObject().asInstanceOf[ArrayType]
    objectInputStream.close
    (map, array)
  }

  protected def norm(array: ArrayType, rowIndex: Int, rowWidth: Int) {
    var offset = rowIndex * rowWidth
    var len = 0.0f

    for (i <- 0 until rowWidth)
      len += array(offset + i) * array(offset + i)
    len = math.sqrt(len).toFloat

    if (len != 0)
      for (i <- 0 until rowWidth)
        array(offset + i) /= len
  }

  protected def buildMatrix(lines: Iterator[String]): BuildType = {
    val linesZipWithIndex = lines.zipWithIndex
    val (wordCount, dimension) =
        if (linesZipWithIndex.hasNext) {
          val bits = linesZipWithIndex.next()._1.split(' ')
          assert(bits.size == 2, "The first line must specify wordCount and dimension.")
          (bits(0).toInt, bits(1).toInt)
        }
        else (0, 0)
    val map = new MutableMapType()
    val array = new ArrayType(wordCount * dimension)
    var duplicates = 0

    for ((line, lineIndex) <- linesZipWithIndex) {
      val bits = line.split(' ')
      assert(bits.length == dimension + 1, s"${bits.length} != ${dimension + 1} found on line ${lineIndex + 1}")
      val word = bits(0)
      val mapIndex =
          if (map.contains(word)) {
            logger.info(s"'${word}' is duplicated in the vector file.")
            // Use space because we will not be looking for words like that.
            // The array will not be filled in for this map.size value.
            map.put(" " + duplicates, map.size)
            duplicates += 1
            map(word)
          }
          else map.size
      assert(mapIndex < wordCount)
      map.put(word, mapIndex)

      val offset = mapIndex * dimension
      for (i <- 0 until dimension)
        array(offset + i) = bits(i + 1).toFloat
      norm(array, mapIndex, dimension)
    }
    assert(map.size == wordCount, s"The file should have had ${map.size} words.")
    (map, array)
  }
}
