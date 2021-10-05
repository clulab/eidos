package org.clulab.wm.ontologies

import org.clulab.processors.Document
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.EnglishTagSet
import org.clulab.wm.eidoscommon.SentencesExtractor
import org.clulab.wm.eidoscommon.StopwordManaging
import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.Namer
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

class NodeTreeDomainOntology(val ontologyNodes: Array[OntologyNode], override val version: Option[String],
    override val date: Option[ZonedDateTime]) extends DomainOntology {

  def size: Integer = ontologyNodes.length

  def getNamer(n: Integer): Namer = ontologyNodes(n)

  def getValues(n: Integer): Array[String] = ontologyNodes(n).getValues

  def isLeaf(n: Integer): Boolean = ontologyNodes(n).isLeaf

//  def getExamples(n: Integer): Option[Array[String]] = ontologyNodes(n).getExamples

  def getPatterns(n: Integer): Option[Array[Regex]] = ontologyNodes(n).getPatterns

  def getNode(n: Integer): OntologyNode = ontologyNodes(n)

  override def save(filename: String): Unit = ???
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
    val yamlNode = YamlNode.parse(loadedYaml)
    val ontologyNode = new OntologyNode(yamlNode, None, 0, sentenceExtractor, canonicalizer, filter)
    val ontologyNodes = ontologyNode.flatten

    ontologyNodes.foreach { ontologyNode =>
      println(ontologyNode.name)
    }

    new NodeTreeDomainOntology(ontologyNodes, versionOpt, dateOpt)
  }
}

class OntologyNode(val yamlNode: YamlNode, val parentOpt: Option[OntologyNode], depth: Int,
    sentenceExtractor: SentencesExtractor, canonicalizer: Canonicalizer, filter: String => Array[String]) extends Namer {
  val (name: String, branch: Option[String]) = {
    val parentName = parentOpt.map(_.name).getOrElse("/")
    val tail = if (isBranch) "/" else ""
    val name = parentName + yamlNode.name + tail
    val branchOpt = depth match {
      case 0 => None
      case 1 => Some(name)
      case _ => parentOpt.get.branch
    }

    (name, branchOpt)
  }
  val children: Array[OntologyNode] = {
    yamlNode.childrenOpt.map {children =>
      children.map { child =>
        new OntologyNode(child, Some(this), depth + 1, sentenceExtractor, canonicalizer, filter)
      }
    }.getOrElse(Array.empty)
  }
  // These are the values that will eventually be used to calculate the vector.
  val values: Array[String] = {
    val filteredName = filter(yamlNode.name.replace('_', ' '))
    val filteredExamples = yamlNode.examplesOpt.getOrElse(Array.empty).flatMap(filter(_))
    val filteredDescriptions = yamlNode.descriptionsOpt.getOrElse(Array.empty).flatMap(filter(_))

    filteredName ++ filteredExamples ++ filteredDescriptions
  }

  def getValues: Array[String] = values

  def isLeaf: Boolean = yamlNode.isLeaf

  def isBranch: Boolean = !isLeaf

  def isRoot: Boolean = parentOpt.isDefined

  def getExamples: Array[String] = yamlNode.examplesOpt.getOrElse(Array.empty)

  lazy val patterns: Option[Array[Regex]] = yamlNode.patternsOpt.map(_.map(_.r))

  def getPatterns: Option[Array[Regex]] = patterns

  def getNode: YamlNode = yamlNode

  protected def flatten(ontologyNodes: ArrayBuffer[OntologyNode]): Unit = {
    ontologyNodes += this
    children.foreach(_.flatten(ontologyNodes))
  }

  def flatten: Array[OntologyNode] = {
    val ontologyNodes = new ArrayBuffer[OntologyNode]()

    flatten(ontologyNodes)
    ontologyNodes.toArray
  }
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

  def getString(node: JLinkedHashMap[String, Any], name: String): String =
      node.get(name).asInstanceOf[String]

  def getStringOpt(node: JLinkedHashMap[String, Any], name: String): Option[String] =
      Option(node.get(name).asInstanceOf[String])

  def getStringsOpt(node: JLinkedHashMap[String, Any], name: String): Option[Array[String]] =
      Option(node.get(name).asInstanceOf[JArrayList[String]]).map(_.asScala.toArray)

  def getIntOpt(node: JLinkedHashMap[String, Any], name: String): Option[Int] =
      // Without the map, null turns into 0.
      Option(node.get(name)).map(_.asInstanceOf[Integer])

  def parse(anyRef: AnyRef): YamlNode = {
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
          parse(child.asInstanceOf[AnyRef])
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

  val flat = builder.buildFromPath("/org/clulab/wm/eidos/english/ontologies/wm_flat_metadata.new.yml")
  val comp = builder.buildFromPath("/org/clulab/wm/eidos/english/ontologies/CompositionalOntology_metadata.new.yml")
}
