package org.clulab.wm.ontologies

import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.IdentityHashMap
import org.clulab.wm.eidoscommon.utils.OptionUtils
import org.clulab.wm.eidoscommon.utils.TsvReader

import java.time.ZonedDateTime
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

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
class CompactDomainOntology(
    protected val leafStrings: Array[String],
    protected val leafStringIndexes: Array[Int],
    protected val leafStartIndexes: Array[Int],

    patternStrings: Array[String],
    protected val patternStartIndexes: Array[Int],

    protected val nodeStrings: Array[String],

    protected val leafIndexes: Array[Int],
    protected val branchIndexes: Array[Int],

    override val versionOpt: Option[String] = None,
    override val dateOpt: Option[ZonedDateTime]
) extends DomainOntology with IndexedDomainOntology with IndexedSeq[IndexedDomainOntologyNode] {
  protected val patternRegexes: Array[Regex] = patternStrings.map(_.r)

  def getValues(n: Integer): Array[String] = {
    Range(leafStartIndexes(n), leafStartIndexes(n + 1))
        .map(n => leafStrings(leafStringIndexes(n)))
        .toArray
  }

  // TODO: This will not always store just the leaves.
  def isLeaf(n: Integer): Boolean = false

  def getPatternsOpt(n: Integer): Option[Array[Regex]] = {
    OptionUtils.someOrNoneIfEmpty(Range(patternStartIndexes(n), patternStartIndexes(n + 1)))
        .map { range =>
          range.map(n => patternRegexes(n)).toArray
        }
  }

  def save(filename: String): Unit = {
    FileUtils.newObjectOutputStream(filename).autoClose { objectOutputStream =>
      val firstLine = Seq(
        versionOpt.getOrElse(""),
        dateOpt.map(_.toString).getOrElse("")
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

  override def nodes: IndexedSeq[IndexedDomainOntologyNode] = this

  override def length: Int = leafIndexes.length / CompactDomainOntology.leafIndexWidth

  override def apply(idx: Int): IndexedDomainOntologyNode = new IndexedDomainOntologyNode(this, idx)

  override def getParentOptOpt(n: Integer): Option[Option[DomainOntologyNode]] = None // unknown

  override def getName(n: Integer): String = {
    val stringBuilder = new StringBuilder()

    def parentName(n: Int): Unit = {
      if (n > 0) {
        val index = n * CompactDomainOntology.branchIndexWidth
        val parentOffset = branchIndexes(index + CompactDomainOntology.parentOffset)
        val nameOffset = branchIndexes(index + CompactDomainOntology.nameOffset)

        parentName(parentOffset)
        stringBuilder.append(DomainOntology.escaped(nodeStrings(nameOffset)))
        stringBuilder.append(DomainOntology.SEPARATOR)
      }
    }

    val index = n * CompactDomainOntology.leafIndexWidth
    val parentOffset = leafIndexes(index + CompactDomainOntology.parentOffset)
    val nameOffset = leafIndexes(index + CompactDomainOntology.nameOffset)

    parentName(parentOffset)
    stringBuilder.append(nodeStrings(nameOffset))
    stringBuilder.result()
  }

  override def getSimpleName(n: Integer): String = {
    val index = n * CompactDomainOntology.leafIndexWidth
    val nameOffset = leafIndexes(index + CompactDomainOntology.nameOffset)

    nodeStrings(nameOffset)
  }

  override def getBranchOpt(n: Integer): Option[String] = {

    def loop(n: Int, prevNameOffset: Int): Option[String] = {
      if (n > 0) {
        val index = n * CompactDomainOntology.branchIndexWidth
        val parentOffset = branchIndexes(index + CompactDomainOntology.parentOffset)

        if (parentOffset == 0)
          if (prevNameOffset >= 0) Some(nodeStrings(prevNameOffset))
          else None
        else {
          val nameOffset = branchIndexes(index + CompactDomainOntology.nameOffset)

          loop(parentOffset, nameOffset)
        }
      }
      else None
    }

    // This will always be run on an n that corresponds to a leaf.
    val index = n * CompactDomainOntology.leafIndexWidth
    val parentOffset = leafIndexes(index + CompactDomainOntology.parentOffset)

    loop(parentOffset, -1)
  }
}

object CompactDomainOntology {
  val branchIndexWidth = 2
  val leafIndexWidth = 2

  val parentOffset = 0
  val nameOffset = 1

  def load(filename: String): CompactDomainOntology = {

    def splitText(text: String): Array[String] = text.split('\n')

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

    protected def append(strings: mutable.Map[String, Int], string: String): Unit =
       if (!strings.contains(string))
          strings.put(string, strings.size)

    protected def mkParentMap(): mutable.Map[HalfOntologyParentNode, (Int, Int)] = {
      // This is myIndex, parentIndex
      val parentMap = IdentityHashMap[HalfOntologyParentNode, (Int, Int)]()

      def append(parents: Seq[HalfOntologyParentNode]): Int =
          if (parents.nonEmpty)
            if (parentMap.contains(parents.head))
              parentMap(parents.head)._1
            else {
              val parentIndex = append(parents.tail) // Put root on top.
              val myIndex = parentMap.size
              parentMap.put(parents.head, (myIndex, parentIndex))
              myIndex
            }
          else
            -1 // This is important!

      treeDomainOntology.nodes.foreach { node => append(node.parents) }
      parentMap
    }

    protected def mkLeafStringMap(): mutable.Map[String, Int] = {
      val stringMap = new mutable.HashMap[String, Int]()

      treeDomainOntology.nodes.foreach { node =>
        node.getValues.foreach(append(stringMap, _))
      }
      stringMap
    }

    protected def mkPatternStringAndStartIndexes(): (Array[String], Array[Int]) = {
      val stringBuffer = new ArrayBuffer[String]()
      val nodes = treeDomainOntology.nodes
      val size = nodes.size
      val startIndexBuffer = new Array[Int](size + 1)

      nodes.zipWithIndex.foreach { case (node, i) =>
        startIndexBuffer(i) = stringBuffer.size

        val optionRegexes = node.getPatternsOpt
        if (optionRegexes.isDefined)
              stringBuffer.appendAll(optionRegexes.get.map(_.toString))
      }
      startIndexBuffer(size) = stringBuffer.size // extra
      (stringBuffer.toArray, startIndexBuffer)
    }

    protected def mkNodeStringMap(parentMap: mutable.Map[HalfOntologyParentNode, (Int, Int)]): mutable.Map[String, Int] = {
      // TODO: Fix this code.  Try to sort entrySet.      
      val stringMap = new mutable.HashMap[String, Int]()
      val parentSeq = parentMap.toSeq.sortBy(_._2)

      parentSeq.foreach { case (ontologyParentNode, _)  =>
        append(stringMap, DomainOntology.escaped(ontologyParentNode.getSimpleName))
      }
      treeDomainOntology.nodes.foreach { node =>
        append(stringMap, DomainOntology.escaped(node.getSimpleName))
      }
      stringMap
    }

    protected def mkLeafStringAndStartIndexes(leafStringMap: mutable.Map[String, Int]): (Array[Int], Array[Int]) = {
      val stringIndexBuffer = new ArrayBuffer[Int]()
      val startIndexBuffer = new ArrayBuffer[Int]()

      treeDomainOntology.nodes.foreach { node =>
        startIndexBuffer += stringIndexBuffer.size
        node.getValues.foreach { value =>
          stringIndexBuffer += leafStringMap(value)
        }
      }
      startIndexBuffer += stringIndexBuffer.size // extra
      (stringIndexBuffer.toArray, startIndexBuffer.toArray)
    }

    protected def mkLeafIndexes(parentMap: mutable.Map[HalfOntologyParentNode, (Int, Int)], stringMap: mutable.Map[String, Int]): Array[Int] = {
      val indexBuffer = new ArrayBuffer[Int]()

      treeDomainOntology.nodes.foreach { node =>
        indexBuffer += parentMap(node.getParentOptOpt.get.get)._1 // parentOffset
        indexBuffer += stringMap(DomainOntology.escaped(node.getSimpleName)) // nameOffset
      }
      indexBuffer.toArray
    }

    protected def mkParentIndexes(parentMap: mutable.Map[HalfOntologyParentNode, (Int, Int)], stringMap: mutable.Map[String, Int]): Array[Int] = {
      val indexBuffer = new ArrayBuffer[Int]()
      val keysAndValues: Array[(HalfOntologyParentNode, (Int, Int))] = parentMap.toArray.sortBy(_._2._1)

      keysAndValues.foreach { case (branchNode, (_, parentIndex)) =>
        indexBuffer += parentIndex // parentOffset
        indexBuffer += stringMap(DomainOntology.escaped(branchNode.getSimpleName)) // nameOffset
      }
      indexBuffer.toArray
    }

    def build(): DomainOntology = {
      val parentMap: mutable.Map[HalfOntologyParentNode, (Int, Int)] = mkParentMap()
      val leafStringMap: mutable.Map[String, Int] = mkLeafStringMap()
      val nodeStringMap: mutable.Map[String, Int] = mkNodeStringMap(parentMap)
      val (leafStringIndexes, leafStartIndexes) = mkLeafStringAndStartIndexes(leafStringMap)
      val (patternStrings, patternStartIndexes) = mkPatternStringAndStartIndexes()
      val leafIndexes = mkLeafIndexes(parentMap, nodeStringMap)
      val branchIndexes = mkParentIndexes(parentMap, nodeStringMap)

      // This sorts by the latter, the Int, and then answers the former, the String.
      def toArray(stringMap: mutable.Map[String, Int]): Array[String] =
          stringMap.toArray.sortBy(_._2).map(_._1)

      val leafStrings: Array[String] = toArray(leafStringMap)
      val nodeStrings: Array[String] = toArray(nodeStringMap)

      new CompactDomainOntology(leafStrings, leafStringIndexes, leafStartIndexes, patternStrings, patternStartIndexes,
          nodeStrings, leafIndexes, branchIndexes, treeDomainOntology.versionOpt, treeDomainOntology.dateOpt)
    }
  }
}
