package org.clulab.wm.ontologies

import org.clulab.processors.Document
import org.clulab.utils.Serializer
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.EnglishTagSet
import org.clulab.wm.eidoscommon.SentencesExtractor
import org.clulab.wm.eidoscommon.StopwordManaging
import org.clulab.wm.eidoscommon.TagSet
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.Namer
import org.clulab.wm.eidoscommon.utils.Resourcer
import org.clulab.wm.ontologies.NodeTreeDomainOntology.NodeTreeDomainOntologyBuilder
import org.json4s.JObject
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import java.time.ZonedDateTime
import java.util.{ArrayList => JArrayList}
import java.util.{Collection => JCollection}
import java.util.{LinkedHashMap => JLinkedHashMap}
import java.util.{Map => JMap}
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

@SerialVersionUID(1000L)
class NodeTreeDomainOntology(val ontologyNodes: Array[OntologyNode], override val version: Option[String], override val date: Option[ZonedDateTime]) extends DomainOntology with Serializable {

  def size: Integer = ontologyNodes.length

  def getNamer(n: Integer): Namer = ontologyNodes(n).namer

  def getValues(n: Integer): Array[String] = ontologyNodes(n).getValues

  def isLeaf(n: Integer): Boolean = ontologyNodes(n).isLeaf

  def getPatterns(n: Integer): Option[Array[Regex]] = ontologyNodes(n).getPatterns

  def getNode(n: Integer): OntologyNode = ontologyNodes(n)

  // TODO: This might not work yet.
  def save(filename: String): Unit = {
    Serializer.save(this, filename)
  }
}

object NodeTreeDomainOntology {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val NODE = "node"
  val NAME = "name"
  val CHILDREN = "children"
  val DESCRIPTIONS = "descriptions"
  val PATTERNS = "patterns"
  val EXAMPLES = "examples"
  val OPPOSITE = "opposite"
  val POLARITY = "polarity"
  val SEMANTIC_TYPE = "semantic type"

  def load(path: String): NodeTreeDomainOntology = {
    logger.info(s"Loading serialized Ontology from $path")
    val domainOntology = FileUtils.load[NodeTreeDomainOntology](path, this)
    logger.info("Serialized Ontology successfully loaded.")
    domainOntology
  }

  // This is mostly here to capture sentenceExtractor so that it doesn't have to be passed around.
  class NodeTreeDomainOntologyBuilder(sentenceExtractor: SentencesExtractor, canonicalizer: Canonicalizer,
      filter: Boolean) {

    def buildFromPath(ontologyPath: String, versionOpt: Option[String] = None, dateOpt: Option[ZonedDateTime] = None):
    NodeTreeDomainOntology = {
      val text = Resourcer.getText(ontologyPath)
      val yaml = new Yaml()
      val nodes = yaml.load(text).asInstanceOf[JLinkedHashMap[String, Any]]
      val node = nodes.get("node").asInstanceOf[JLinkedHashMap[String, Any]]
      val rootNode = parseOntology(node)
      // Flatten to get all the nodes and convert them all, not just this one.
      //      val nodes = Array(new OntologyNode(rootNode))
      //      new NodeTreeDomainOntology(nodes, versionOpt, dateOpt)
      null
    }

    /*
    protected def parseOntology(parent: YamlNode, yamlNodes: mutable.Map[String, JCollection[Any]]): YamlNode = {
      /* We're going without the names for now. */
      val name = yamlNodes(FullTreeDomainOntology.NAME).asInstanceOf[String]
      /*val names = (name +: parent.nodeName +: parent.parents.map(_.nodeName)).map(unescape)*/
      val examples = yamlNodesToStrings(yamlNodes, FullTreeDomainOntology.EXAMPLES)
          .map { examples => examples.filter(_ != null) }
      val descriptions: Option[Array[String]] = yamlNodesToStrings(yamlNodes, FullTreeDomainOntology.DESCRIPTION)
      // The incoming polarity can now be Int or Double.  We will store either one as a Float.
      val polarity = {
        // There's something wrong with this type system, obviously.  This is legacy code.
        val yamlNodesOpt: Option[JCollection[Any]] = yamlNodes.get(FullTreeDomainOntology.POLARITY)

        yamlNodesOpt.map { yamlNode: Any =>
          yamlNode match {
            case value: Double => value.toFloat
            case value: Int => value.toFloat
            case _ => throw new Exception(s"Unexpected polarity value: $yamlNode!")
          }
        }.getOrElse(1.0f) // positive by default
      }
      val patterns: Option[Array[Regex]] = yamlNodesToRegexes(yamlNodes, FullTreeDomainOntology.PATTERN)

      /*val filteredNames = names.flatMap(filtered)*/
      val filteredExamples = examples.map(_.flatMap(filtered))
      val filteredDescriptions = descriptions.map(_.flatMap(filtered))

//      println("Adding new node")
      // Note: leaf nodes are created here
      new FullOntologyLeafNode(name, parent, polarity, /*filteredNames,*/ filteredExamples, filteredDescriptions, patterns)
    }
*/
    /*
    protected def parseOntology(yamlNode: , parentNodeOpt: Option[YamlNode] = None, level: Int = 0): YamlNode = {
      // Probably need to get the node first.
      val name = (jObject \ "name").asInstanceOf[String]
      val yamlNode = new YamlNode(parentNodeOpt, name, None, None, None, None, None, None, None)

      yamlNode
    }
*/
    // This any should be a "node"
    protected def parseOntology(node: JLinkedHashMap[String, Any]): YamlNode = {
      val name = node.get("name").asInstanceOf[String]
      val childrenOpt = {
        val childrenOpt = Option(node.get("children")).map(_.asInstanceOf[JArrayList[Any]].asScala)
        val more = childrenOpt.map { children =>
          val result = children.map { child =>
            val node = child.asInstanceOf[JLinkedHashMap[String, Any]].get("node").asInstanceOf[JLinkedHashMap[String, Any]]
            parseOntology(node) // Need something about parent?
          }.toArray
          result
        }
        more
      }

      val result = new YamlNode(name, childrenOpt, None, None, None, None, None, None)
      result
    }
  }
}

class OntologyNode(val yamlNode: YamlNode) { // add parent here

  class OntologyNodeNamer() extends Namer {
    val (name, branch) = {
      val parents = getParents
      val tail = if (yamlNode.isBranch) "/" else ""
      val name = parents.mkString("/", "/", tail)
      val branchOpt = parents.lift(1).map(_.name)

      (name, branchOpt)
    }
  }

  val namer = new OntologyNodeNamer()

  val values: Array[String] = {
    // Calculate values here, just once if possible.
    val values = Array.empty[String]

    values
  }

  def getNamer: Namer = namer

  def getValues: Array[String] = values

  def isLeaf: Boolean = yamlNode.isLeaf

  def getExamples: Array[String] = yamlNode.examplesOpt.getOrElse(Array.empty)

  // Convert these to regexes here, lazily if necessary
  def getPatterns: Option[Array[Regex]] = yamlNode.patternsOpt.map(_.map(_.r))

  def getNode: YamlNode = yamlNode

  def getParents: List[YamlNode] = {
//    @tailrec
    def recGetParents(yamlNode: YamlNode, parents: List[YamlNode]): List[YamlNode] = {
//      if (yamlNode.isRoot) (yamlNode :: parents)
//      else recGetParents(yamlNode.parentOpt.get, yamlNode :: parents)
      List.empty
    }

    recGetParents(yamlNode, Nil)
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

  def isBranch: Boolean = !isLeaf
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
  val builder = new NodeTreeDomainOntologyBuilder(sentenceExtractor, canonicalizer, filter = false)

  val flat = builder.buildFromPath("/org/clulab/wm/eidos/english/ontologies/wm_flat_metadata.new.yml")
  val comp = builder.buildFromPath("/org/clulab/wm/eidos/english/ontologies/CompositionalOntology_metadata.new.yml")
}
