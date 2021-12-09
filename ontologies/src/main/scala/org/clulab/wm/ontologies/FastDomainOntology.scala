package org.clulab.wm.ontologies

import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.OptionUtils
import org.clulab.wm.eidoscommon.utils.TsvReader

import java.time.ZonedDateTime
import java.util
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{HashMap => MutableHashMap}
import scala.util.matching.Regex

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
    val parents: Array[Int],
    leaves: Array[Boolean],
    wordIndexes: Array[Int],
    wordStartIndexes: Array[Int],
    patterns: Array[String],
    patternStartIndexes: Array[Int],
    //  childIndexes: Array[Int],
    //  childStartIndexes: Array[Int],
    wordStringArr: Array[String],
    override val versionOpt: Option[String] = None,
    override val dateOpt: Option[ZonedDateTime] = None
) extends DomainOntology with IndexedDomainOntology with IndexedSeq[DomainOntologyNode] {

  protected val patternRegexes: Array[Regex] = patterns.map(_.r)

  def getValues(n: Integer): Array[String] = {
    Range(wordStartIndexes(n), wordStartIndexes(n + 1))
        .map(n => wordStringArr(wordIndexes(n)))
        .toArray
  }

  def isLeaf(n: Integer): Boolean = leaves(n)

  protected def isRoot(n: Int): Boolean = parents(n) < 0

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

  override def nodes: IndexedSeq[DomainOntologyNode] = this

  override def length: Int = names.length

  override def apply(index: Int): DomainOntologyNode = new IndexedDomainOntologyNode(this, index)

  override def getParentOptOpt(n: Integer): Option[Option[DomainOntologyNode]] =
      Some(
        if (isRoot(n)) None
        else Some(new IndexedDomainOntologyNode(this, parents(n)))
      )

  def getName(n: Integer): String = {
    val stringBuilder = new StringBuilder()

    def parentName(n: Int): Unit = {
      if (!isRoot(n))
        parentName(parents(n))
      stringBuilder.append(names(n))
      stringBuilder.append(DomainOntology.SEPARATOR)
    }

    parentName(parents(n))
    stringBuilder.append(names(n))
    if (!leaves(n))
      stringBuilder.append(DomainOntology.SEPARATOR)
    stringBuilder.toString
  }

  def getSimpleName(n: Integer): String = names(n)

  def getBranchOpt(n: Integer): Option[String] = {

    def branch(n: Int, prevN: Int): Option[String] = {
      if (isRoot(n)) Some(names(prevN))
      else branch(parents(n), n)
    }

    if (isRoot(n)) None // The top one isn't in a branch yet.
    else branch(parents(n), n)
  }
}

// This wraps the above FastDomainOntology and allows for offset nodes to be skipped.
// For example, the top level wm node shouldn't be grounded to.  It's possible that
// for the compositional grounding even the next level will be skipped.
class SkipDomainOntology(fastDomainOntology: FastDomainOntology, offset: Int = 1)
    extends DomainOntology with IndexedDomainOntology with IndexedSeq[DomainOntologyNode] {
  // TODO Doesn't this need to store the offset as well for serialization?

  def getValues(n: Integer): Array[String] = fastDomainOntology.getValues(n + offset)

  def getPatternsOpt(n: Integer): Option[Array[Regex]] = fastDomainOntology.getPatternsOpt(n + offset)

  def isLeaf(n: Integer): Boolean = fastDomainOntology.isLeaf(n + offset)

  override def save(filename: String): Unit = fastDomainOntology.save(filename)

  override def nodes: IndexedSeq[DomainOntologyNode] = this

  override def length: Int = fastDomainOntology.size - offset

  override def apply(idx: Int): DomainOntologyNode = new IndexedDomainOntologyNode(this, idx)

  override def getParentOptOpt(n: Integer): Option[Option[DomainOntologyNode]] =
    Some(
      if (n > offset) Some(new IndexedDomainOntologyNode(this, fastDomainOntology.parents(n + offset)))
      else None
    )

  override def getName(n: Integer): String = fastDomainOntology.getName(n + offset)

  override def getSimpleName(n: Integer): String = fastDomainOntology.getSimpleName(n + offset)

  override def getBranchOpt(n: Integer): Option[String] = fastDomainOntology.getBranchOpt(n + offset)
}

object FastDomainOntology {

  protected def loadFast(filename: String): FastDomainOntology = {

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
        node.getPatternsOpt.foreach { patterns =>
          patterns.foreach { pattern: Regex => append(stringMap, pattern.toString) }
        }
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
        val indexes = node.getPatternsOpt.map { patterns =>
          patterns.map { pattern: Regex => pattern.toString }
        }.getOrElse(Array.empty)

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
      val rootNode = treeDomainOntology.nodes.head.parents.last
      val nodeMap: util.IdentityHashMap[FullOntologyNode, Int] = mkNodeMap(rootNode)
      val nodeArr: Array[FullOntologyNode] = nodeMap
          .entrySet()
          .asScala
          .toSeq
          .sortBy(_.getValue)
          .map(_.getKey)
          .toArray
      val names = nodeArr.map { node => DomainOntology.escaped(node.getSimpleName) }
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
        treeDomainOntology.versionOpt, treeDomainOntology.dateOpt)
    }

    def build(): DomainOntology = {
      val fastDomainOntology = buildFast()
      val skipDomainOntology = new SkipDomainOntology(fastDomainOntology, 1)

      skipDomainOntology
    }
  }
}
