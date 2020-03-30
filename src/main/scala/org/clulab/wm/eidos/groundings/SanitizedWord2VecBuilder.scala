package org.clulab.wm.eidos.groundings

import java.io.BufferedInputStream
import java.io.FileInputStream

import org.clulab.embeddings.word2vec.CompactWord2Vec
import org.clulab.utils.ClassLoaderObjectInputStream
import org.clulab.utils.Closer.AutoCloser
import org.clulab.utils.Sourcer
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import scala.collection.immutable.HashMap
import scala.collection.mutable.{HashMap => MutableHashMap, Map => MutableMap}

// Except for the types which are broken out into the object, the apply method which has been
// renamed to build, and sanitizer value, this is copied verbatim from CompactWord2Vec
// and is only here because some critical methods were marked protected there and one can't
// inherit from an object or use protected object members from the derived subclass.  The object
// should have been a class, a CompactWord2VecBuilder.  That is remedied here.
class SanitizedWord2VecBuilder {
  protected val logger: Logger = LoggerFactory.getLogger(classOf[CompactWord2Vec])

  val sanitizer: Sanitizer = new Sanitizer()

  def build(filename: String, resource: Boolean = true, cached: Boolean = false): SanitizedWord2Vec = {
    logger.trace("Started to load word2vec matrix from file " + filename + "...")
    val buildType =
      if (cached) loadBin(filename)
      else loadTxt(filename, resource)
    logger.trace("Completed word2vec matrix loading.")
    new SanitizedWord2Vec(buildType, sanitizer)
  }

  protected def loadTxt(filename: String, resource: Boolean): SanitizedWord2VecBuilder.BuildType = {
    (
        if (resource) Sourcer.sourceFromResource(filename)
        else Sourcer.sourceFromFile(filename)
        ).autoClose { source =>
      val lines = source.getLines()

      buildMatrix(lines)
    }
  }

  protected def loadBin(filename: String): SanitizedWord2VecBuilder.BuildType = {
    // This is the original code
    //    val (text, array) = updatedLoad[StoreType](filename, this)
    //    val words = text.split('\n')
    //    val map: MapType = words.zipWithIndex.toMap.asInstanceOf[MapType]
    //    (map, array)

    // This is "unrolled" for performance purposes.
    new ClassLoaderObjectInputStream(this.getClass.getClassLoader, new BufferedInputStream(new FileInputStream(filename))).autoClose { objectInputStream =>
      val map: SanitizedWord2VecBuilder.MapType = new SanitizedWord2VecBuilder.MutableMapType()

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

      val array = objectInputStream.readObject().asInstanceOf[SanitizedWord2VecBuilder.ArrayType]
      (map, array)
    }
  }

  protected def buildMatrix(lines: Iterator[String]): SanitizedWord2VecBuilder.BuildType = {

    def norm(array: SanitizedWord2VecBuilder.ArrayType, rowIndex: Int, columns: Int): Unit = {
      val offset = rowIndex * columns
      var len = 0.asInstanceOf[CompactWord2Vec.ValueType] // optimization
      var i = 0 // optimization

      while (i < columns) {
        len += array(offset + i) * array(offset + i)
        i += 1
      }
      len = math.sqrt(len).asInstanceOf[SanitizedWord2VecBuilder.ValueType]

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
    var map = new SanitizedWord2VecBuilder.ImplementationMapType()
    val array = new SanitizedWord2VecBuilder.ArrayType(wordCount * columns)

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
        array(offset + i) = bits(i + 1).toDouble.asInstanceOf[SanitizedWord2VecBuilder.ValueType]
        i += 1
      }
      norm(array, row, columns)
    }
    assert(map.size == wordCount, s"The file should have had ${map.size} words.")
    (map, array)
  }
}

object SanitizedWord2VecBuilder {
  protected type MutableMapType = MutableHashMap[String, Int]
  protected type ImmutableMapType = HashMap[String, Int]

  protected type ImplementationMapType = MutableMapType // optimization

  // These were meant to allow easy switching between implementations.
  type MapType = MutableMap[String, Int]
  type ValueType = Float
  type ArrayType = Array[ValueType]

  type BuildType = (MapType, ArrayType)
  protected type StoreType = (String, ArrayType)

  val emptyMap = new MutableMapType()
}
