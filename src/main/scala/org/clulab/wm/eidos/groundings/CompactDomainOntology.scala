package org.clulab.wm.eidos.groundings

import java.time.ZonedDateTime
import java.util

import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Namer
import org.clulab.wm.eidos.utils.TsvReader

import scala.collection.JavaConverters._
import scala.collection.mutable.{HashMap => MutableHashMap}
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

class CompactNamerData(val nodeStrings: Array[String], val leafIndexes: Array[Int], val branchIndexes: Array[Int])

class CompactNamer(protected val n: Int, data: CompactNamerData) extends Namer {

  protected def branch(n: Int, prevNameOffset: Int): Option[String] = {
    if (n > 0) {
      val index = n * CompactDomainOntology.branchIndexWidth
      val parentOffset = data.branchIndexes(index + CompactDomainOntology.parentOffset)

      if (parentOffset == 0)
        if (prevNameOffset >= 0) Some(data.nodeStrings(prevNameOffset))
        else None
      else {
        val nameOffset = data.branchIndexes(index + CompactDomainOntology.nameOffset)

        branch(parentOffset, nameOffset)
      }
    }
    else None
  }

  def branch: Option[String] = {
    // This will always be run on an n that corresponds to a leaf.
    val index = n * CompactDomainOntology.leafIndexWidth
    val parentOffset = data.leafIndexes(index + CompactDomainOntology.parentOffset)

    branch(parentOffset, -1)
  }

  protected def parentName(n: Int, stringBuilder: StringBuilder): Unit = {
    if (n > 0) {
      val index = n * CompactDomainOntology.branchIndexWidth
      val parentOffset = data.branchIndexes(index + CompactDomainOntology.parentOffset)
      val nameOffset = data.branchIndexes(index + CompactDomainOntology.nameOffset)

      parentName(parentOffset, stringBuilder)
      stringBuilder.append(data.nodeStrings(nameOffset))
      stringBuilder.append(DomainOntology.SEPARATOR)
    }
  }

  def name: String = {
    val stringBuilder = new StringBuilder()
    val index = n * CompactDomainOntology.leafIndexWidth
    val parentOffset = data.leafIndexes(index + CompactDomainOntology.parentOffset)
    val nameOffset = data.leafIndexes(index + CompactDomainOntology.nameOffset)

    parentName(parentOffset, stringBuilder)
    stringBuilder.append(data.nodeStrings(nameOffset))
    stringBuilder.result()
  }
}

/**
  * Provide a DomainOntology interface on top of the Arrays of String and Int values.
  *
  * @param leafStrings All the strings used in the leaves of the ontology
  * @param leafStringIndexes Indexes into leafStrings sorted by leaf node
  * @param leafStartIndexes Where to start in leafStringIndexes to find the indexes for leaf node N
  * @param patternStrings All the regex strings used in the leaves of the ontology
  * @param patternStartIndexes Where to start in patternStrings to find the patterns for leaf node N
  * @param nodeStrings All the strings used in the non-leaf nodes of the ontology
  * @param leafIndexes Parent offset, name offset, parent offset, name offset, ...  for leaves only
  *                    Name offset is into nodeStrings, parent offset is into branchIndexes.
  * @param branchIndexes Parent offset, name offset, parent offset, name offset, ... for non-leaves only
  *                      Name offset is into nodeStrings, parent offset is back into branchIndexes.
  */
class CompactDomainOntology(protected val leafStrings: Array[String], protected val leafStringIndexes: Array[Int], protected val leafStartIndexes: Array[Int],
    patternStrings: Array[String], protected val patternStartIndexes: Array[Int], protected val nodeStrings: Array[String], protected val leafIndexes: Array[Int], protected val branchIndexes: Array[Int],
    override val version: Option[String] = None, override val date: Option[ZonedDateTime]) extends DomainOntology {

  def size: Integer = leafIndexes.length / CompactDomainOntology.leafIndexWidth

  protected val namerData: CompactNamerData = new CompactNamerData(nodeStrings, leafIndexes, branchIndexes)
  protected val patternRegexes: Array[Regex] = patternStrings.map(_.r)

  // This is done so that other data can be thrown away
  def getNamer(n: Integer): Namer = new CompactNamer(n, namerData)

  def getValues(n: Integer): Array[String] = {
    val start = leafStartIndexes(n)
    val stop = leafStartIndexes(n + 1)

    start.until(stop).toArray.map(n => leafStrings(leafStringIndexes(n)))
  }

  // TODO: This will not always store just the leaves.
  def isLeaf(n: Integer): Boolean = false

  def getPatterns(n: Integer): Option[Array[Regex]] = {
    val start = patternStartIndexes(n)
    val stop = patternStartIndexes(n + 1)

    if (start == stop)
      None
    else
      Some(start.until(stop).toArray.map(n => patternRegexes(n)))
  }

  def save(filename: String): Unit = {
    FileUtils.newObjectOutputStream(filename).autoClose { objectOutputStream =>
      val firstLine = Seq(
        version.getOrElse(""),
        date.map(_.toString).getOrElse("")
      ).mkString("\t") // Some versions of ZonedDateTime.toString can contain spaces.
      objectOutputStream.writeObject(firstLine)
      objectOutputStream.writeObject(leafStrings.mkString("\n"))
      objectOutputStream.writeObject(leafStringIndexes)
      objectOutputStream.writeObject(leafStartIndexes)
      objectOutputStream.writeObject(patternStrings.mkString("\n"))
      objectOutputStream.writeObject(patternStartIndexes)
      objectOutputStream.writeObject(nodeStrings.mkString("\n"))
      objectOutputStream.writeObject(leafIndexes)
      objectOutputStream.writeObject(branchIndexes)
    }
  }
}

object CompactDomainOntology {
  val branchIndexWidth = 2
  val leafIndexWidth = 2

  val parentOffset = 0
  val nameOffset = 1

  // This is so that text can be abandoned at the end of the block, before the array is read.
  protected def splitText(text: String): Array[String] = {
    val arrayBuffer = new ArrayBuffer[String]()
    val stringBuilder = new StringBuilder
    
    for (i <- 0 until text.length) {
      val c = text(i)

      if (c == '\n') {
        arrayBuffer += stringBuilder.result()
        stringBuilder.clear()
      }
      else
        stringBuilder.append(c)
    }
    arrayBuffer += stringBuilder.result()
    arrayBuffer.toArray
  }

  def load(filename: String): CompactDomainOntology = {
    FileUtils.newClassLoaderObjectInputStream(filename, this).autoClose { objectInputStream =>
      val (versionOpt: Option[String], dateOpt: Option[ZonedDateTime]) = {
        val firstLine = objectInputStream.readObject().asInstanceOf[String]
        val tsvReader = new TsvReader()
        val Array(commit, date) = tsvReader.readln(firstLine)
        val commitOpt = if (commit.nonEmpty) Some(commit) else None
        val dateOpt = if (date.nonEmpty) Some(ZonedDateTime.parse(date)) else None

        (commitOpt, dateOpt)
      }
      val leafStrings = splitText(objectInputStream.readObject().asInstanceOf[String])
      val leafStringIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]
      val leafStartIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]
      val patternStrings = splitText(objectInputStream.readObject().asInstanceOf[String])
      val patternStartIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]
      val nodeStrings = splitText(objectInputStream.readObject().asInstanceOf[String])
      val leafIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]
      val branchIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]

      new CompactDomainOntology(leafStrings, leafStringIndexes, leafStartIndexes, patternStrings, patternStartIndexes,
          nodeStrings, leafIndexes, branchIndexes, versionOpt, dateOpt)
    }
  }

  class CompactDomainOntologyBuilder(treeDomainOntology: HalfTreeDomainOntology) {

    protected def append(strings: MutableHashMap[String, Int], string: String): Unit =
       if (!strings.contains(string))
          strings.put(string, strings.size)

    protected def mkParentMap(): util.IdentityHashMap[HalfOntologyParentNode, (Int, Int)] = {
      // This is myIndex, parentIndex
      val parentMap: util.IdentityHashMap[HalfOntologyParentNode, (Int, Int)] = new util.IdentityHashMap()

      def append(parents: Seq[HalfOntologyParentNode]): Int =
          if (parents.nonEmpty)
            if (parentMap.containsKey(parents.head))
              parentMap.get(parents.head)._1
            else {
              val parentIndex = append(parents.tail) // Put root on top.
              val myIndex = parentMap.size
              parentMap.put(parents.head, (myIndex, parentIndex))
              myIndex
            }
          else
            -1

      0.until(treeDomainOntology.size).foreach { i =>
        append(treeDomainOntology.getParents(i))
      }
      parentMap
    }

    protected def mkLeafStringMap(): MutableHashMap[String, Int] = {
      val stringMap: MutableHashMap[String, Int] = new MutableHashMap()

      0.until(treeDomainOntology.size).foreach { i =>
        treeDomainOntology.getValues(i).foreach(append(stringMap, _))
      }
      stringMap
    }

    protected def mkPatternStringAndStartIndexes(): (Array[String], Array[Int]) = {
      val stringBuffer = new ArrayBuffer[String]()
      val startIndexBuffer = new Array[Int](treeDomainOntology.size + 1)

      0.until(treeDomainOntology.size).foreach { i =>
        startIndexBuffer(i) = stringBuffer.size

        val optionRegexes = treeDomainOntology.getPatterns(i)
        if (optionRegexes.isDefined)
              stringBuffer.appendAll(optionRegexes.get.map(_.toString))
      }
      startIndexBuffer(treeDomainOntology.size) = stringBuffer.size // extra
      (stringBuffer.toArray, startIndexBuffer)
    }

    protected def mkNodeStringMap(parentMap: util.IdentityHashMap[HalfOntologyParentNode, (Int, Int)]): MutableHashMap[String, Int] = {
      // TODO: Fix this code.  Try to sort entrySet.      
      val stringMap: MutableHashMap[String, Int] = new MutableHashMap()
      val parentSeq = parentMap
          .entrySet
          .asScala
          .toSeq
          .map { entrySet => (entrySet.getKey, entrySet.getValue) }
          .sortBy(_._2)

      parentSeq.foreach { case (ontologyParentNode, _)  =>
        append(stringMap, ontologyParentNode.escaped)
      }
      0.until(treeDomainOntology.size).foreach { i =>
        append(stringMap, treeDomainOntology.getNode(i).escaped)
      }
      stringMap
    }

    protected def mkLeafStringAndStartIndexes(leafStringMap: MutableHashMap[String, Int]): (Array[Int], Array[Int]) = {
      val stringIndexBuffer = new ArrayBuffer[Int]()
      val startIndexBuffer = new ArrayBuffer[Int]()

      0.until(treeDomainOntology.size).foreach { i =>
        startIndexBuffer += stringIndexBuffer.size
        treeDomainOntology.getValues(i).foreach { value =>
          stringIndexBuffer += leafStringMap(value)
        }
      }
      startIndexBuffer += stringIndexBuffer.size // extra
      (stringIndexBuffer.toArray, startIndexBuffer.toArray)
    }

    protected def mkLeafIndexes(parentMap: util.IdentityHashMap[HalfOntologyParentNode, (Int, Int)], stringMap: MutableHashMap[String, Int]): Array[Int] = {
      val indexBuffer = new ArrayBuffer[Int]()

      0.until(treeDomainOntology.size).foreach { i =>
        val node: HalfOntologyLeafNode = treeDomainOntology.getNode(i)

        indexBuffer += parentMap.get(node.parent)._1 // parentOffset
        indexBuffer += stringMap(node.escaped) // nameOffset
      }
      indexBuffer.toArray
    }

    protected def mkParentIndexes(parentMap: util.IdentityHashMap[HalfOntologyParentNode, (Int, Int)], stringMap: MutableHashMap[String, Int]): Array[Int] = {
      val indexBuffer = new ArrayBuffer[Int]()
      val keysAndValues: Array[(HalfOntologyParentNode, (Int, Int))] = parentMap.asScala.toArray.sortBy(_._2._1)

      keysAndValues.foreach { case (branchNode, (_, parentIndex)) =>
        indexBuffer += parentIndex // parentOffset
        indexBuffer += stringMap(branchNode.escaped) // nameOffset
      }
      indexBuffer.toArray
    }

    def build(): DomainOntology = {
      val parentMap: util.IdentityHashMap[HalfOntologyParentNode, (Int, Int)] = mkParentMap()
      val leafStringMap: MutableHashMap[String, Int] = mkLeafStringMap()
      val nodeStringMap: MutableHashMap[String, Int] = mkNodeStringMap(parentMap)
      val (leafStringIndexes, leafStartIndexes) = mkLeafStringAndStartIndexes(leafStringMap)
      val (patternStrings, patternStartIndexes) = mkPatternStringAndStartIndexes()
      val leafIndexes = mkLeafIndexes(parentMap, nodeStringMap)
      val branchIndexes = mkParentIndexes(parentMap, nodeStringMap)

      // This sorts by the latter, the Int, and then answers the former, the String.
      def toArray(stringMap:MutableHashMap[String, Int]): Array[String] =
          stringMap.toArray.sortBy(_._2).map(_._1)

      val leafStrings: Array[String] = toArray(leafStringMap)
      val nodeStrings: Array[String] = toArray(nodeStringMap)

      new CompactDomainOntology(leafStrings, leafStringIndexes, leafStartIndexes, patternStrings, patternStartIndexes,
          nodeStrings, leafIndexes, branchIndexes, treeDomainOntology.version, treeDomainOntology.date)
    }
  }
}
