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

class FastNamerData(val names: Array[String], val parents: Array[Int], val leaves: Array[Boolean])

class FastNamer(protected val n: Int, data: FastNamerData) extends Namer {

  protected def branch(n: Int, prevN: Int): Option[String] = {
    if (isTop(n)) Some(data.names(prevN))
    else branch(data.parents(n), n)
  }

  def branch: Option[String] = {
    if (isTop(n)) None // The top one isn't in a branch yet.
    else branch(data.parents(n), n)
  }

  protected def isTop(n: Int): Boolean = data.parents(n) < 0

  protected def parentName(n: Int, stringBuilder: StringBuilder): Unit = {
    if (!isTop(n))
      parentName(data.parents(n), stringBuilder)
    stringBuilder.append(data.names(n))
    stringBuilder.append(DomainOntology.SEPARATOR)
  }

  def name: String = {
    val stringBuilder = new StringBuilder()

    parentName(data.parents(n), stringBuilder)
    stringBuilder.append(data.names(n))
    if (!data.leaves(n))
      stringBuilder.append(DomainOntology.SEPARATOR)
    stringBuilder.toString
  }
}

/**
 * Provide a DomainOntology interface on top of the Arrays of String and Int values.
 *
 * @param names Local names for each node.  Size is number of nodes.
 * @param parents Indexes of each of the parent nodes for node at this index.
 * @param leaves Booleans for whether node N is a leaf
 * @param wordIndexes At position N the index into wordStringArr of a word
 * @param wordStartIndexes At position N the index into wordIndexes of the start of words for node N
 * @param patterns All the actual patterns, which aren't mapped.
 * @param patternStartIndexes Similarly to wordStartIndexes but index into patterns of the start of patterns for node N
 * @param wordStringArr All the words//
 */

//* @param childIndexes Similar to above two, but indexes to children of a node
//* @param childStartIndexes Similar to above two, but index into childIndexes of the start of children for node N

class FastDomainOntology(
  names: Array[String],
  parents: Array[Int],
  leaves: Array[Boolean],
  wordIndexes: Array[Int],
  wordStartIndexes: Array[Int],
  patterns: Array[String],
  patternStartIndexes: Array[Int],
//  childIndexes: Array[Int],
//  childStartIndexes: Array[Int],
  wordStringArr: Array[String],
  override val version: Option[String] = None,
  override val date: Option[ZonedDateTime]
) extends DomainOntology {

  def size: Integer = names.length

  protected val namerData: FastNamerData = new FastNamerData(names, parents, leaves)
  protected val patternRegexes: Array[Regex] = patterns.map(_.r)

  // This is done so that other data can be thrown away
  def getNamer(n: Integer): Namer = new FastNamer(n, namerData)

  def getValues(n: Integer): Array[String] = {
    val start = wordStartIndexes(n)
    val stop = wordStartIndexes(n + 1)

    start.until(stop).toArray.map(n => wordStringArr(wordIndexes(n)))
  }

  def getPatterns(n: Integer): Option[Array[Regex]] = {
    val start = patternStartIndexes(n)
    val stop = patternStartIndexes(n + 1)

    if (start == stop) None
    else Some(start.until(stop).toArray.map(n => patternRegexes(n)))
  }

  def save(filename: String): Unit = {
    FileUtils.newObjectOutputStream(filename).autoClose { objectOutputStream =>
      val firstLine = Seq(
        version.getOrElse(""),
        date.map(_.toString).getOrElse("")
      ).mkString("\t") // Some versions of ZonedDateTime.toString can contain spaces.
      objectOutputStream.writeObject(firstLine)
      objectOutputStream.writeObject(names.mkString("\n"))
      objectOutputStream.writeObject(parents)
      objectOutputStream.writeObject(leaves)
      objectOutputStream.writeObject(wordIndexes)
      objectOutputStream.writeObject(wordStartIndexes)
      objectOutputStream.writeObject(patterns.mkString("\n"))
      objectOutputStream.writeObject(patternStartIndexes)
//      objectOutputStream.writeObject(childIndexes)
//      objectOutputStream.writeObject(childStartIndexes)
      objectOutputStream.writeObject(wordStringArr.mkString("\n"))
    }
  }

  def isLeaf(n: Integer): Boolean = leaves(n)
}

// This wraps the above FastDomainOntology and allows for offset nodes to be skipped.
// For example, the top level wm node shouldn't be grounded to.  It's possible that
// for the compositional grounding even the next level will be skipped.
class SkipDomainOntology(fastDomainOntology: FastDomainOntology, offset: Int = 1) extends DomainOntology {
  override def size: Integer = fastDomainOntology.size - offset

  override def getNamer(n: Integer): Namer = fastDomainOntology.getNamer(n + offset)

  override def getValues(n: Integer): Array[String] = fastDomainOntology.getValues(n + offset)

  override def getPatterns(n: Integer): Option[Array[Regex]] = fastDomainOntology.getPatterns(n + offset)

  override def isLeaf(n: Integer): Boolean = fastDomainOntology.isLeaf(n + offset)

  override def save(filename: String): Unit = fastDomainOntology.save(filename)
}

object FastDomainOntology {

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

  protected def loadFast(filename: String): FastDomainOntology = {
    FileUtils.newClassLoaderObjectInputStream(filename, this).autoClose { objectInputStream =>
      val (versionOpt: Option[String], dateOpt: Option[ZonedDateTime]) = {
        val firstLine = objectInputStream.readObject().asInstanceOf[String]
        val tsvReader = new TsvReader()
        val Array(commit, date) = tsvReader.readln(firstLine)
        val commitOpt = if (commit.nonEmpty) Some(commit) else None
        val dateOpt = if (date.nonEmpty) Some(ZonedDateTime.parse(date)) else None

        (commitOpt, dateOpt)
      }
      val names = splitText(objectInputStream.readObject().asInstanceOf[String])
      val parents = objectInputStream.readObject().asInstanceOf[Array[Int]]
      val leaves = objectInputStream.readObject().asInstanceOf[Array[Boolean]]
      val wordIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]
      val wordStartIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]
      val patterns = splitText(objectInputStream.readObject().asInstanceOf[String])
      val patternStartIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]
//      val childIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]
//      val childStartIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]
      val wordStringArr = splitText(objectInputStream.readObject().asInstanceOf[String])

      new FastDomainOntology(names, parents, leaves, wordIndexes, wordStartIndexes, patterns, patternStartIndexes,
        /*childIndexes, childStartIndexes,*/ wordStringArr, versionOpt, dateOpt)
    }
  }

  def load(filename: String): DomainOntology = {
    val fastDomainOntology = loadFast(filename)
    val skipDomainOntology = new SkipDomainOntology(fastDomainOntology, 1)

    skipDomainOntology
  }

  class FastDomainOntologyBuilder(treeDomainOntology: FullTreeDomainOntology) {

    protected def append(strings: MutableHashMap[String, Int], string: String): Unit =
      if (!strings.contains(string))
        strings.put(string, strings.size)

    // Number all of the nodes by making map of node to number.
    protected def mkNodeMap(rootNode: FullOntologyNode): util.IdentityHashMap[FullOntologyNode, Int] = {
      val nodeMap: util.IdentityHashMap[FullOntologyNode, Int] = new util.IdentityHashMap()

      def append(node: FullOntologyNode): Unit =
        node.childrenOpt.foreach { children =>
          // Do this depth first, child before its own children.
          children.foreach { child =>
            nodeMap.put(child, nodeMap.size())
            append(child)
          }
        }

      append(rootNode)
      nodeMap
    }

    protected def mkWordStringMap(nodes: Seq[FullOntologyNode]): MutableHashMap[String, Int] = {
      val stringMap: MutableHashMap[String, Int] = new MutableHashMap()

      nodes.foreach { node =>
        node.getValues.foreach(append(stringMap, _))
      }
      stringMap
    }

    protected def mkPatternStringMap(nodes: Seq[FullOntologyNode]): MutableHashMap[String, Int] = {
      val stringMap: MutableHashMap[String, Int] = new MutableHashMap()

      nodes.foreach { node =>
        node.getPatterns.foreach { pattern => append(stringMap, pattern.toString) }
      }
      stringMap
    }

    protected def mkWordIndexesAndStarts(nodes: Seq[FullOntologyNode], stringMap: MutableHashMap[String, Int]): (Array[Int], Array[Int]) = {
      val indexBuffer = new ArrayBuffer[Int]()
      val startIndexBuffer = new Array[Int](nodes.size + 1)

      nodes.zipWithIndex.foreach { case (node, index) =>
        val indexes = node.getValues.map { value =>
          stringMap(value)
        }

        startIndexBuffer(index) = indexBuffer.size
        indexBuffer.appendAll(indexes)
      }
      startIndexBuffer(nodes.size) = indexBuffer.size // extra
      (indexBuffer.toArray, startIndexBuffer)
    }

    protected def mkPatternsAndStarts(nodes: Seq[FullOntologyNode]): (Array[String], Array[Int]) = {
      val indexBuffer = new ArrayBuffer[String]()
      val startIndexBuffer = new Array[Int](nodes.size + 1)

      nodes.zipWithIndex.foreach { case (node, index) =>
        val indexes = node.getPatterns.map { pattern =>
          pattern.toString
        }

        startIndexBuffer(index) = indexBuffer.size
        indexBuffer.appendAll(indexes)
      }
      startIndexBuffer(nodes.size) = indexBuffer.size // extra
      (indexBuffer.toArray, startIndexBuffer)
    }

    protected def mkChildIndexesAndStarts(nodes: Seq[FullOntologyNode], nodeMap: util.IdentityHashMap[FullOntologyNode, Int]):
    (Array[Int], Array[Int]) = {
      val indexBuffer = new ArrayBuffer[Int]()
      val startIndexBuffer = new Array[Int](nodes.size + 1)

      nodes.zipWithIndex.foreach { case (node, index) =>
        val indexes = node.getChildren.map { child =>
          nodeMap.get(child)
        }

        startIndexBuffer(index) = indexBuffer.size
        indexBuffer.appendAll(indexes)
      }
      startIndexBuffer(nodes.size) = indexBuffer.size // extra
      (indexBuffer.toArray, startIndexBuffer)
    }

    def buildFast(): FastDomainOntology = {
      // This stops at, for example, wm, not the implied root above.  It is not an OntologyRootNode.
      val rootNode = treeDomainOntology.getNode(0).parents.last
      val nodeMap: util.IdentityHashMap[FullOntologyNode, Int] = mkNodeMap(rootNode)
      val nodeArr: Array[FullOntologyNode] = nodeMap
          .entrySet()
          .asScala
          .toSeq
          .sortBy(_.getValue)
          .map(_.getKey)
          .toArray
      val names = nodeArr.map { node => node.escaped }
      val leaves = nodeArr.map { node => node.isLeaf }
      val parents = nodeArr.map { node =>
        Option(nodeMap.get(node.parentOpt.get)).getOrElse(-1)
      }
      val wordStringMap: MutableHashMap[String, Int] = mkWordStringMap(nodeArr)
      val wordStringArr = wordStringMap.toSeq.map(_.swap).sorted.map(_._2).toArray
      val (wordIndexes, wordStartIndexes) = mkWordIndexesAndStarts(nodeArr, wordStringMap)
      val (patterns, patternStartIndexes) = mkPatternsAndStarts(nodeArr)
//      val (childIndexes, childStartIndexes) = mkChildIndexesAndStarts(nodeArr, nodeMap)

      new FastDomainOntology(names, parents, leaves, wordIndexes, wordStartIndexes,
        patterns, patternStartIndexes, /*childIndexes, childStartIndexes,*/ wordStringArr,
        treeDomainOntology.version, treeDomainOntology.date)
    }

    def build(): DomainOntology = {
      val fastDomainOntology = buildFast()
      val skipDomainOntology = new SkipDomainOntology(fastDomainOntology, 1)

      skipDomainOntology
    }
  }
}
