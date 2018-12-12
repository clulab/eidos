package org.clulab.wm.eidos.groundings

import java.io.{FileOutputStream, ObjectOutputStream}
import java.util.IdentityHashMap

import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Namer

import scala.collection.JavaConverters._
import scala.collection.mutable.{HashMap => MutableHashMap}
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

class CompactNamerData(val nodeStrings: Array[String], val leafIndexes: Array[Int], val branchIndexes: Array[Int])

class CompactNamer(protected val n: Int, data: CompactNamerData) extends Namer {

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
    patternStrings: Array[String], protected val patternStartIndexes: Array[Int], protected val nodeStrings: Array[String], protected val leafIndexes: Array[Int], protected val branchIndexes: Array[Int]) extends DomainOntology {

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

  def getPatterns(n: Integer): Option[Array[Regex]] = {
    val start = patternStartIndexes(n)
    val stop = patternStartIndexes(n + 1)

    if (start == stop)
      None
    else
      Some(start.until(stop).toArray.map(n => patternRegexes(n)))
  }

  def save(filename: String): Unit = {
    new ObjectOutputStream(new FileOutputStream(filename)).autoClose { objectOutputStream =>
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
      val leafStrings = splitText(objectInputStream.readObject().asInstanceOf[String])
      val leafStringIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]
      val leafStartIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]
      val patternStrings = splitText(objectInputStream.readObject().asInstanceOf[String])
      val patternStartIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]
      val nodeStrings = splitText(objectInputStream.readObject().asInstanceOf[String])
      val leafIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]
      val branchIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]

      new CompactDomainOntology(leafStrings, leafStringIndexes, leafStartIndexes, patternStrings, patternStartIndexes, nodeStrings, leafIndexes, branchIndexes)
    }
  }

  class CompactDomainOntologyBuilder(treeDomainOntology: TreeDomainOntology) {

    protected def append(strings: MutableHashMap[String, Int], string: String): Unit =
       if (!strings.contains(string))
          strings.put(string, strings.size)

    protected def mkParentMap(): IdentityHashMap[OntologyParentNode, (Int, Int)] = {
      // This is myIndex, parentIndex
      val parentMap: IdentityHashMap[OntologyParentNode, (Int, Int)] = new IdentityHashMap()

      def append(parents: Seq[OntologyParentNode]): Int =
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

    protected def mkNodeStringMap(parentMap: IdentityHashMap[OntologyParentNode, (Int, Int)]): MutableHashMap[String, Int] = {
      val stringMap: MutableHashMap[String, Int] = new MutableHashMap()

      parentMap.keySet().asScala.foreach { ontologyParentNode =>
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

    protected def mkLeafIndexes(parentMap: IdentityHashMap[OntologyParentNode, (Int, Int)], stringMap: MutableHashMap[String, Int]): Array[Int] = {
      val indexBuffer = new ArrayBuffer[Int]()

      0.until(treeDomainOntology.size).foreach { i =>
        val node = treeDomainOntology.getNode(i)

        indexBuffer += parentMap.get(node.parent)._1 // parentOffset
        indexBuffer += stringMap(node.escaped) // nameOffset
      }
      indexBuffer.toArray
    }

    protected def mkParentIndexes(parentMap: IdentityHashMap[OntologyParentNode, (Int, Int)], stringMap: MutableHashMap[String, Int]): Array[Int] = {
      val indexBuffer = new ArrayBuffer[Int]()
      val keysAndValues: Array[(OntologyParentNode, (Int, Int))] = parentMap.asScala.toArray.sortBy(_._2._1)

      keysAndValues.foreach { case (branchNode, (_, parentIndex)) =>
        indexBuffer += parentIndex // parentOffset
        indexBuffer += stringMap(branchNode.escaped) // nameOffset
      }
      indexBuffer.toArray
    }

    def build(): DomainOntology = {
      val parentMap: IdentityHashMap[OntologyParentNode, (Int, Int)] = mkParentMap()
      val leafStringMap: MutableHashMap[String, Int] = mkLeafStringMap()
      val nodeStringMap: MutableHashMap[String, Int] = mkNodeStringMap(parentMap)
      val (leafStringIndexes, leafStartIndexes) = mkLeafStringAndStartIndexes(leafStringMap)
      val (patternStrings, patternStartIndexes) = mkPatternStringAndStartIndexes()
      val leafIndexes = mkLeafIndexes(parentMap, nodeStringMap)
      val branchIndexes = mkParentIndexes(parentMap, nodeStringMap)

      def toArray(stringMap:MutableHashMap[String, Int]): Array[String] =
          stringMap.toArray.sortBy(_._2).map(_._1)

      val leafStrings: Array[String] = toArray(leafStringMap)
      val nodeStrings: Array[String] = toArray(nodeStringMap)

      new CompactDomainOntology(leafStrings, leafStringIndexes, leafStartIndexes, patternStrings, patternStartIndexes, nodeStrings, leafIndexes, branchIndexes)
    }
  }
}
