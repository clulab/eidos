package org.clulab.wm.eidos.groundings

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.{FileInputStream, FileOutputStream, ObjectOutputStream}

import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.{ClassLoaderObjectInputStream, Sourcer}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.immutable.HashMap
import scala.collection.mutable.{HashMap => MutableHashMap, Map => MutableMap}

/**
  * This class and its companion object have been backported from Eidos.  There it is/was an optional
  * replacementfor Word2Vec used for performance reasons.  It loads data faster from disk and stores it
  * more compactly in memory.  It does not, however, include all the operations of processer's Word2Vec.
  * For instance, logMultiplicativeTextSimilarity is not included, but could probably be added.  Other
  * methods like getWordVector, which in Word2Vec returns an Array[Double], would be inefficient to
  * include because the arrays of doubles (or floats) are no longer part of the design.  For more
  * documentation other than that immediately below, both the companion object and the related test case
  * (org.clulab.embeddings.word2vec.TestCompactWord2Vec) may be helpful.
  *
  * The class is typically instantiated by the apply method of the companion object which takes as
  * arguments a filename and then two booleans: "resource", which specifies whether the named file
  * exists as a resource or is alternatively stored on the broader filesystem, and "cached", which
  * specifies that the data consists of Java-serialized objects (see the save method) or, alternatively,
  * the standard vector text format.  The apply method arranges for the file to be read in the appropriate
  * way and converted into a map with the words being keys with values being the row numbers in an implied
  * 2-dimentional matrix of the all vector values, also included in the constructor.  So, rather than each
  * word being mapped to an independent, mini array as in Word2Vec, they are mapped to an integer row
  * number of a single, larger matrix/array.
  *
  * To take advantage of the faster load times, the vector data file needs to be converted from text
  * format into a binary (Java serialized objects) for loadBin below.  The test case includes an example.
  * In some preprocessing phase, call CompactWord2Vec(filename, resource = false, cached = false)
  * on the file containing the vectors in text format, such as glove.840B.300d.txt.  "resource" is
  * usually false because it can be a very large file, too large to include as a resource.  On the
  * resulting return value, call save(compactFilename).  Thereafter, for normal, speedy processing,
  * use CompactWord2Vec(compactFilename, resource = false, cached = true).
  */
class CompactWord2Vec(buildType: CompactWord2Vec.BuildType) {
  protected val map: CompactWord2Vec.MapType = buildType._1 // (word -> row)
  protected val array: CompactWord2Vec.ArrayType = buildType._2 // flattened matrix
  // The empty string will map to the vector for OOV words.  If there is no such vector,
  // the result will be None, which should match previous behavior.
  protected val unknownRowOpt: Option[Int] = map.get("")
  val columns: Int = array.length / map.size
  val rows: Int = array.length / columns

  def get(word: String): Option[CompactWord2Vec.ArrayType] = { // debug use only
    val bestRowOpt = map.get(word) // Prefer the version with matching case.
    val betterRowOpt = bestRowOpt.orElse(map.get(word.toLowerCase)) // Make do with lowercase.
    val goodRowOpt = betterRowOpt.orElse(unknownRowOpt) // Resort to unknown.
// needs to be good
    bestRowOpt.map { row =>
      val offset = row * columns

      array.slice(offset, offset + columns)
    }
  }

  def keys: Iterable[String] = map.keys // debug use only

  def save(filename: String): Unit = {
    // Sort the map entries (word -> row) by row and then keep just the word.
    val words = map.toArray.sortBy(_._2).map(_._1).mkString("\n")

    new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(filename))).autoClose { objectOutputStream =>
      // Writing is performed in two steps so that the parts can be
      // processed separately when read back in.
      objectOutputStream.writeObject(words)
      objectOutputStream.writeObject(array)
    }
  }

  def dotProduct(row1: Int, row2: Int): CompactWord2Vec.ValueType = {
    val offset1 = row1 * columns
    val offset2 = row2 * columns
    var sum = 0.asInstanceOf[CompactWord2Vec.ValueType] // optimization
    var i = 0 // optimization

    while (i < columns) {
      sum += array(offset1 + i) * array(offset2 + i)
      i += 1
    }
    sum
  }

  protected def add(dest: CompactWord2Vec.ArrayType, srcRow: Int): Unit = {
    val srcOffset = srcRow * columns
    var i = 0 // optimization

    while (i < columns) {
      dest(i) += array(srcOffset + i)
      i += 1
    }
  }

  // These are no longer automatically sanitized.
  def isOutOfVocabulary(word: String): Boolean = !map.contains(word)

  // Normalize this vector to length 1, in place.
  // (If the length is zero, do nothing.)
  protected def norm(array: CompactWord2Vec.ArrayType): CompactWord2Vec.ArrayType = {
    var len = 0.asInstanceOf[CompactWord2Vec.ValueType] // optimization
    var i = 0 // optimization

    while (i < array.length) {
      len += array(i) * array(i)
      i += 1
    }
    len = math.sqrt(len).asInstanceOf[CompactWord2Vec.ValueType]

    if (len != 0) {
      i = 0
      while (i < array.length) {
        array(i) /= len
        i += 1
      }
    }
    array
  }

  def makeCompositeVector(texts: Iterable[String]): CompactWord2Vec.ArrayType = {
    val total = new CompactWord2Vec.ArrayType(columns)

    texts.foreach { word =>
      map.get(word).foreach { index => add(total, index) }
    }
    norm(total)
  }

  // Find the average word2vec similarity between any two words in these two texts.
  // IMPORTANT: words here must be words not lemmas!
  def avgSimilarity(text1: Iterable[String], text2: Iterable[String]): CompactWord2Vec.ValueType = {
    val sanitizedText1 = text1.map(Word2VecUtils.sanitizeWord(_))
    val sanitizedText2 = text2.map(Word2VecUtils.sanitizeWord(_))

    sanitizedAvgSimilarity(sanitizedText1, sanitizedText2)
  }

  // Find the average word2vec similarity between any two words in these two texts.
  protected def sanitizedAvgSimilarity(text1: Iterable[String], text2: Iterable[String]): CompactWord2Vec.ValueType = {
    var avg = 0.asInstanceOf[CompactWord2Vec.ValueType] // optimization
    var count = 0 // optimization

    for (word1 <- text1) {
      val row1 = map.get(word1)

      if (row1.isDefined) {
        for (word2 <- text2) {
          val row2 = map.get(word2)

          if (row2.isDefined) {
            avg += dotProduct(row1.get, row2.get)
            count += 1
          }
        }
      }
    }
    if (count != 0) avg / count
    else 0
  }
}

object CompactWord2Vec {
  protected type MutableMapType = MutableHashMap[String, Int]
  protected type ImmutableMapType = HashMap[String, Int]

  protected type ImplementationMapType = MutableMapType // optimization

  // These were meant to allow easy switching between implementations.
  type MapType = MutableMap[String, Int]
  type ValueType = Float
  type ArrayType = Array[ValueType]

  protected type BuildType = (MapType, ArrayType)
  protected type StoreType = (String, ArrayType)

  protected val logger: Logger = LoggerFactory.getLogger(classOf[CompactWord2Vec])

  def apply(filename: String, resource: Boolean = true, cached: Boolean = false): CompactWord2Vec = {
    logger.trace("Started to load word2vec matrix from file " + filename + "...")
    val buildType =
      if (cached) loadBin(filename)
      else loadTxt(filename, resource)
    logger.trace("Completed word2vec matrix loading.")
    new CompactWord2Vec(buildType)
  }

  protected def loadTxt(filename: String, resource: Boolean): BuildType = {
    (
      if (resource) Sourcer.sourceFromResource(filename)
      else Sourcer.sourceFromFile(filename)
    ).autoClose { source =>
      val lines = source.getLines()

      buildMatrix(lines)
    }
  }

  protected def loadBin(filename: String): BuildType = {
    // This is the original code
    //    val (text, array) = updatedLoad[StoreType](filename, this)
    //    val words = text.split('\n')
    //    val map: MapType = words.zipWithIndex.toMap.asInstanceOf[MapType]
    //    (map, array)

    // This is "unrolled" for performance purposes.
    new ClassLoaderObjectInputStream(this.getClass.getClassLoader, new BufferedInputStream(new FileInputStream(filename))).autoClose { objectInputStream =>
      val map: MapType = new MutableMapType()

    {
      // This block is so that text can be abandoned at the end of the block, before the array is read.
      val text = objectInputStream.readObject().asInstanceOf[String]
      val stringBuilder = new StringBuilder

      for (i <- 0 until text.length) {
        val c = text(i)

        if (c == '\n') {
          map += ((stringBuilder.result(), map.size))
          stringBuilder.clear()
        }
        else
          stringBuilder.append(c)
      }
      map += ((stringBuilder.result(), map.size))
    }

      val array = objectInputStream.readObject().asInstanceOf[ArrayType]
      (map, array)
    }
  }

  protected def buildMatrix(lines: Iterator[String]): BuildType = {

    def norm(array: ArrayType, rowIndex: Int, columns: Int): Unit = {
      val offset = rowIndex * columns
      var len = 0.asInstanceOf[CompactWord2Vec.ValueType] // optimization
      var i = 0 // optimization

      while (i < columns) {
        len += array(offset + i) * array(offset + i)
        i += 1
      }
      len = math.sqrt(len).asInstanceOf[ValueType]

      if (len != 0) {
        i = 0
        while (i < columns) {
          array(offset + i) /= len
          i += 1
        }
      }
    }

    val linesZipWithIndex = lines.zipWithIndex
    val (wordCount, columns) =
      if (linesZipWithIndex.hasNext) {
        val bits = linesZipWithIndex.next()._1.split(' ')

        assert(bits.length == 2, "The first line must specify wordCount and dimension.")
        (bits(0).toInt, bits(1).toInt)
      }
      else (0, 0)
    var map = new ImplementationMapType()
    val array = new ArrayType(wordCount * columns)

    for ((line, lineIndex) <- linesZipWithIndex) {
      val bits = line.split(' ')
      assert(bits.length == columns + 1, s"${bits.length} != ${columns + 1} found on line ${lineIndex + 1}")
      val word = bits(0)
      val row =
        if (map.contains(word)) {
          logger.info(s"'$word' is duplicated in the vector file.")
          // Use space because we will not be looking for words like that.
          // The array will not be filled in for this map.size value.
          map += (" " + map.size -> map.size)
          map(word)
        }
        else map.size
      assert(row < wordCount)
      map += (word -> row)

      val offset = row * columns
      var i = 0 // optimization

      while (i < columns) {
        array(offset + i) = bits(i + 1).toDouble.asInstanceOf[ValueType]
        i += 1
      }
      norm(array, row, columns)
    }
    assert(map.size == wordCount, s"The file should have had ${map.size} words.")
    (map, array)
  }
}
