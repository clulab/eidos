package org.clulab.wm.ontologies

import org.clulab.processors.Document
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.EnglishTagSet
import org.clulab.wm.eidoscommon.SentencesExtractor
import org.clulab.wm.eidoscommon.StopwordManaging
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.yaml.snakeyaml.Yaml

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._
import scala.util.matching.Regex
import java.io.InputStream
import java.time.ZonedDateTime
import java.util.{ArrayList => JArrayList}
import java.util.{LinkedHashMap => JLinkedHashMap}

class NodeTreeDomainOntology(
  ontologyNodes: Array[NodeTreeDomainOntologyNode],
  version: Option[String], date: Option[ZonedDateTime]
) extends VersionedDomainOntology(version, date) {

  override def save(filename: String): Unit = ???

  override def nodes: IndexedSeq[DomainOntologyNode] = ontologyNodes
}

class NodeTreeDomainOntologyBuilder(sentenceExtractor: SentencesExtractor, canonicalizer: Canonicalizer, filtered: Boolean) {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  protected def realFilter(text: String): Array[String] =
      DomainOntology.canonicalWordsFromSentence(sentenceExtractor, canonicalizer, text).toArray

  protected def fakeFilter(text: String): Array[String] = text.split(" +")

  protected val filter: String => Array[String] = if (filtered) realFilter else fakeFilter

  def buildFromPath(ontologyPath: String, versionOpt: Option[String] = None, dateOpt: Option[ZonedDateTime] = None):
      NodeTreeDomainOntology = {
    FileUtils.newResourceInputStream(ontologyPath, this).autoClose { inputStream =>
      buildFromStream(inputStream, versionOpt, dateOpt)
    }
  }

  def buildFromStream(inputStream: InputStream, versionOpt: Option[String] = None, dateOpt: Option[ZonedDateTime] = None):
      NodeTreeDomainOntology = {
    val loadedYaml = new Yaml().load(inputStream)
    val headNode = YamlNode.getHeadNode(loadedYaml)
    val yamlNode = YamlNode.parse(headNode)
    val ontologyNode = new NodeTreeDomainOntologyNode(yamlNode, None, 0, sentenceExtractor, canonicalizer, filter)
    val ontologyNodes = ontologyNode.flatten.tail // Skip the top one.

//    ontologyNodes.foreach { ontologyNode =>
//      println(s"${ontologyNode.name} ${ontologyNode.getValues.mkString(", ")}")
//    }
    new NodeTreeDomainOntology(ontologyNodes, versionOpt, dateOpt)
  }
}

class NodeTreeDomainOntologyNode(
  val yamlNode: YamlNode, val parentOpt: Option[NodeTreeDomainOntologyNode], depth: Int,
  sentenceExtractor: SentencesExtractor, canonicalizer: Canonicalizer, filter: String => Array[String]
) extends DomainOntologyNode {
  val (name: String, branch: Option[String]) = {
    val parentName = parentOpt.map(_.name).getOrElse("")
    val tail = if (isBranch) "/" else ""
    val name = parentName + DomainOntology.escaped(yamlNode.name) + tail
    val branchOpt = depth match {
      case 0 => None                 // The top level has no branch.
      case 1 => Some(yamlNode.name)  // The branch is determined by the first level
      case _ => parentOpt.get.branch // and it applies to all lower levels.
    }

    (name, branchOpt)
  }
  val children: Array[NodeTreeDomainOntologyNode] = {
    yamlNode.childrenOpt.map {children =>
      children.map { child =>
        new NodeTreeDomainOntologyNode(child, Some(this), depth + 1, sentenceExtractor, canonicalizer, filter)
      }
    }.getOrElse(Array.empty)
  }
  // These are the values that will eventually be used to calculate the vector.
  protected val values: Array[String] = {
    if (isBranch) {
      // When we finally get metadata on branches, this will need to change.
      filter(yamlNode.name.replace('_', ' '))
    }
    else {
      val filteredExamples = yamlNode.examplesOpt.getOrElse(Array.empty).flatMap(filter(_))
      val filteredDescriptions = yamlNode.descriptionsOpt.getOrElse(Array.empty).flatMap(filter(_))

      filteredExamples ++ filteredDescriptions
    }
  }

  def getValues: Array[String] = values

  def isLeaf: Boolean = yamlNode.isLeaf

  def isBranch: Boolean = !isLeaf

  def isRoot: Boolean = parentOpt.isEmpty

  override def getExamplesOpt: Option[Array[String]] = yamlNode.examplesOpt

  lazy val patterns: Option[Array[Regex]] = yamlNode.patternsOpt.map(_.map(DomainOntology.toRegex))

  def getPatternsOpt: Option[Array[Regex]] = patterns

  def getNode: YamlNode = yamlNode

  def flatten: Array[NodeTreeDomainOntologyNode] = {
    val ontologyNodes = new ArrayBuffer[NodeTreeDomainOntologyNode]()

    def recFlatten(ontologyNode: NodeTreeDomainOntologyNode): Unit = {
      ontologyNodes += ontologyNode
      ontologyNode.children.foreach(recFlatten)
    }

    recFlatten( this)
    ontologyNodes.toArray
  }

  def getParentOptOpt: Option[Option[NodeTreeDomainOntologyNode]] = Some(parentOpt)

  override def getName: String = name

  override def getSimpleName: String = yamlNode.name
}

case class YamlNode(
  name: String,
  childrenOpt: Option[Array[YamlNode]],

  descriptionsOpt: Option[Array[String]],
  patternsOpt: Option[Array[String]],
  examplesOpt: Option[Array[String]],
  oppositeOpt: Option[String],
  polarityOpt: Option[Int],
  semanticTypeOpt: Option[String]
) {

  def isLeaf: Boolean = childrenOpt.isEmpty || childrenOpt.get.isEmpty
}

object YamlNode {
  val NODE = "node"
  val NAME = "name"
  val CHILDREN = "children"
  val DESCRIPTIONS = "descriptions"
  val PATTERNS = "patterns"
  val EXAMPLES = "examples"
  val OPPOSITE = "opposite"
  val POLARITY = "polarity"
  val SEMANTIC_TYPE = "semantic type"

  def getNode(any: Any): JLinkedHashMap[String, Any] = any
      .asInstanceOf[JLinkedHashMap[String, Any]]
      .get(YamlNode.NODE)
      .asInstanceOf[JLinkedHashMap[String, Any]]

  def getHeadNode(any: Any): JLinkedHashMap[String, Any] = any
      .asInstanceOf[JArrayList[Any]]
      .get(0)
      .asInstanceOf[JLinkedHashMap[String, Any]]

  def getString(node: JLinkedHashMap[String, Any], name: String): String =
      node.get(name).asInstanceOf[String]

  def getStringOpt(node: JLinkedHashMap[String, Any], name: String): Option[String] =
      Option(node.get(name).asInstanceOf[String])

  def getStringsOpt(node: JLinkedHashMap[String, Any], name: String): Option[Array[String]] =
      Option(node.get(name).asInstanceOf[JArrayList[String]]).map(_.asScala.toArray)

  def getIntOpt(node: JLinkedHashMap[String, Any], name: String): Option[Int] =
      // Without the map, null turns into 0.
      Option(node.get(name)).map(_.asInstanceOf[Integer])

  def parse(anyRef: Any): YamlNode = {
    val node = YamlNode.getNode(anyRef)
    val name = YamlNode.getString(node, NAME)
    val descriptionsOpt = YamlNode.getStringsOpt(node, DESCRIPTIONS)
    val patternsOpt = YamlNode.getStringsOpt(node, PATTERNS)
    val examplesOpt = YamlNode.getStringsOpt(node, EXAMPLES)
    val oppositeOpt = YamlNode.getStringOpt(node, OPPOSITE)
    val polarityOpt = YamlNode.getIntOpt(node, POLARITY)
    val semanticTypeOpt = YamlNode.getStringOpt(node, SEMANTIC_TYPE)
    val childrenOpt = Option(node.get(CHILDREN).asInstanceOf[JArrayList[Any]])
      .map(_.asScala)
      .map { children =>
        children.map { child =>
          parse(child)
        }.toArray
      }

    new YamlNode(name, childrenOpt, descriptionsOpt, patternsOpt, examplesOpt, oppositeOpt, polarityOpt, semanticTypeOpt)
  }
}

object NodeTreeDomainOntologyApp extends App {
  val sentenceExtractor = new SentencesExtractor {
    override def extractDocument(text: String): Document = ???
  }
  val stopwordManager = new StopwordManaging {
    override def containsStopwordNer(stopword: String): Boolean = ???
    override def containsStopword(stopword: String): Boolean = ???
  }
  val tagSet = new EnglishTagSet()
  val canonicalizer = new Canonicalizer(stopwordManager, tagSet)
  val builder = new NodeTreeDomainOntologyBuilder(sentenceExtractor, canonicalizer, filtered = false)

  val flat = builder.buildFromPath("/org/clulab/wm/eidos/english/ontologies/wm_flat_metadata.yml")
  val comp = builder.buildFromPath("/org/clulab/wm/eidos/english/ontologies/CompositionalOntology_metadata.yml")
}
