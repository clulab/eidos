package org.clulab.wm.eidos.groundings

import java.time.ZonedDateTime
import java.util.{Collection => JCollection, Map => JMap}

import org.clulab.utils.Serializer
import org.clulab.wm.eidos.SentencesExtractor
import org.clulab.wm.eidos.utils.FileUtils.getTextFromResource
import org.clulab.wm.eidos.utils.{Canonicalizer, FileUtils, Namer}
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

@SerialVersionUID(1000L)
abstract class FullOntologyNode(val nodeName: String, var parentOpt: Option[FullOntologyParentNode], var childrenOpt: Option[Seq[FullOntologyNode]] = None,
    val values: Option[Array[String]] = None, val patterns: Option[Array[Regex]] = None) extends Namer with Serializable {
  // At this level there is no distinction made between a parent node and child node.
  // Parent and children are var so that they can be assigned at different times and after object creation.

  // There can already be a / in any of the stages of the route that must be escaped.
  // First, double up any existing backslashes, then escape the forward slashes with backslashes.
  def escaped(nodeName: String): String =
      nodeName
          .replace(DomainOntology.ESCAPE, DomainOntology.ESCAPED_ESCAPE)
          .replace(DomainOntology.SEPARATOR, DomainOntology.ESCAPED_SEPARATOR)

  def parents(parent: FullOntologyParentNode): Seq[FullOntologyParentNode] = parent +: parent.parents

  def fullName: String
  def parents: Seq[FullOntologyParentNode]
  def escaped: String

  override def toString: String = fullName

  def branch: Option[String]

  def isRoot: Boolean = false

  def isLeaf: Boolean = false

  def name: String = fullName

  def getValues: Array[String] = values.getOrElse(Array.empty)

  def getPatterns: Array[Regex] = patterns.getOrElse(Array.empty)

  def getChildren: Seq[FullOntologyNode] = childrenOpt.getOrElse(Seq.empty)
}

@SerialVersionUID(1000L)
abstract class FullOntologyParentNode(nodeName: String, parentOpt: Option[FullOntologyParentNode]) extends FullOntologyNode(nodeName, parentOpt) {
  def isParentRoot: Boolean
}

@SerialVersionUID(1000L)
class FullOntologyRootNode extends FullOntologyParentNode("", None) {

  override def fullName: String = ""

  override def parents: Seq[FullOntologyParentNode] = Seq.empty

  override def escaped: String = ""

  def branch: Option[String] = None

  override def isRoot: Boolean = true

  def isParentRoot: Boolean = false
}

class FullOntologyBranchNode(nodeName: String, parent: FullOntologyParentNode, filtered: String => Seq[String]) extends FullOntologyParentNode(nodeName, Some(parent)) {

  override def fullName: String = parentOpt.get.fullName + escaped + DomainOntology.SEPARATOR

  // These come out in order parent, grandparent, great grandparent, etc. by design
  override def parents: Seq[FullOntologyParentNode] = parents(parentOpt.get)

  override def escaped: String = escaped(nodeName)

  override def isRoot: Boolean = false

  def isParentRoot: Boolean = parent.isRoot

  def branch: Option[String] =
      if (parent.isParentRoot) Some(nodeName)
      else parent.branch

  override def getValues: Array[String] = {
    val value = nodeName.replace('_', ' ')
    val values = filtered(value)

    values.toArray
  }

  override def getPatterns: Array[Regex] = super.getPatterns
}

@SerialVersionUID(1000L)
class FullOntologyLeafNode(
  nodeName: String,
  val parent: FullOntologyParentNode,
  polarity: Float,
  /*names: Seq[String],*/
  examples: Option[Array[String]] = None,
  descriptions: Option[Array[String]] = None,
  override val patterns: Option[Array[Regex]] = None
) extends FullOntologyNode(nodeName, Some(parent), None, Some(/*names ++*/ examples.getOrElse(Array.empty) ++ descriptions.getOrElse(Array.empty)), patterns) with Namer {

  override def fullName: String = parentOpt.get.fullName + escaped

  def branch: Option[String] = parent.branch

  override def toString: String = fullName // + " = " + values.toList

  // These come out in order parent, grandparent, great grandparent, etc. by design
  override def parents: Seq[FullOntologyParentNode] = parents(parentOpt.get)

  override def escaped: String = escaped(nodeName)

  override def isLeaf: Boolean = true
}

@SerialVersionUID(1000L)
class FullTreeDomainOntology(val ontologyNodes: Array[FullOntologyNode], override val version: Option[String], override val date: Option[ZonedDateTime]) extends DomainOntology with Serializable {

  def size: Integer = ontologyNodes.length

  def getNamer(n: Integer): Namer = ontologyNodes(n)

  def getValues(n: Integer): Array[String] = ontologyNodes(n).getValues

  def isLeaf(n: Integer): Boolean = ontologyNodes(n).isLeaf

  def getPatterns(n: Integer): Option[Array[Regex]] = ontologyNodes(n).patterns

  def getNode(n: Integer): FullOntologyNode = ontologyNodes(n)

  // It is assumed that the root node, for which parentOpt is None, is not in the list.
  def getParents(n: Integer): Seq[FullOntologyParentNode] = ontologyNodes(n).parentOpt.get +: ontologyNodes(n).parentOpt.get.parents

  def save(filename: String): Unit = {
    Serializer.save(this, filename)
  }
}

object FullTreeDomainOntology {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val FIELD = "OntologyNode"
  val NAME = "name"
  val EXAMPLES = "examples"
  val DESCRIPTION = "descriptions"
  val POLARITY = "polarity"
  val PATTERN = "pattern"

  def load(path: String): FullTreeDomainOntology = {
    logger.info(s"Loading serialized Ontology from $path")
    val domainOntology = FileUtils.load[FullTreeDomainOntology](path, this)
    logger.info("Serialized Ontology successfully loaded.")
    domainOntology
  }

  // This is mostly here to capture sentenceExtractor so that it doesn't have to be passed around.
  class FullTreeDomainOntologyBuilder(sentenceExtractor: SentencesExtractor, canonicalizer: Canonicalizer,
      filter: Boolean) {

    def buildFromPath(ontologyPath: String, versionOpt: Option[String] = None, dateOpt: Option[ZonedDateTime] = None):
        FullTreeDomainOntology = buildFromYaml(getTextFromResource(ontologyPath), versionOpt, dateOpt)

    def buildFromYaml(yamlText: String, versionOpt: Option[String] = None, dateOpt: Option[ZonedDateTime] = None): FullTreeDomainOntology = {
      val yaml = new Yaml(new Constructor(classOf[JCollection[Any]]))
      val yamlNodes = yaml.load(yamlText).asInstanceOf[JCollection[Any]].asScala.toSeq
      val rootNode = new FullOntologyRootNode // Note: root node is created here

      parseOntology(rootNode, yamlNodes)

      def walk(node: FullOntologyNode, f: FullOntologyNode => Unit): Unit = {
        node.childrenOpt.map { children =>
          children.map { child =>
            f(child)
            walk(child, f)
          }
        }
      }

      val (ontologyParentNodes, ontologyChildNodes) = {
        val parents = new ArrayBuffer[FullOntologyNode]
        val children = new ArrayBuffer[FullOntologyNode]

        walk(rootNode, { node: FullOntologyNode =>
          if (node.isLeaf) children.append(node)
          else if (!node.parentOpt.get.isInstanceOf[FullOntologyRootNode])
            parents.append(node)
        })

        (parents.toArray, children.toArray)
      }

      val includedNodes = ontologyParentNodes ++ ontologyChildNodes
      new FullTreeDomainOntology(includedNodes, versionOpt, dateOpt)
    }

    protected def realFiltered(text: String): Seq[String] =
        DomainOntology.canonicalWordsFromSentence(sentenceExtractor, canonicalizer, text)

    protected def fakeFiltered(text: String): Seq[String] = text.split(" +")

    protected val filtered: String => Seq[String] = if (filter) realFiltered else fakeFiltered

    protected def yamlNodesToStrings(yamlNodes: mutable.Map[String, JCollection[Any]], name: String): Option[Array[String]] =
      yamlNodes.get(name).map(_.asInstanceOf[JCollection[String]].asScala.toArray)

    // Used to match against specific regular expressions for ontology nodes
    protected def yamlNodesToRegexes(yamlNodes: mutable.Map[String, JCollection[Any]], name: String): Option[Array[Regex]] = {
      yamlNodesToStrings(yamlNodes, name) match {
        case Some(regexes) => Some(regexes.map(rx => s"(?i)$rx".r))
        case None => None
      }
    }

    protected def unescape(name: String): String = {
      // Sometimes the words in names are concatenated with _
      // TODO: We should avoid this practice
      name.replace('_', ' ')
    }

    protected def parseOntology(parent: FullOntologyParentNode, yamlNodes: mutable.Map[String, JCollection[Any]]): FullOntologyLeafNode = {
      /* We're going without the names for now. */
      val name = yamlNodes(FullTreeDomainOntology.NAME).asInstanceOf[String]
      /*val names = (name +: parent.nodeName +: parent.parents.map(_.nodeName)).map(unescape)*/
      val examples = yamlNodesToStrings(yamlNodes, FullTreeDomainOntology.EXAMPLES)
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

    protected def parseOntology(parent: FullOntologyParentNode, yamlNodes: Seq[Any], level: Int = 0): Unit = {
      // This is a hack used because map doesn't work below, so I resort to foreach and have to build the result somewhere else.
      val childNodesSeq = new ArrayBuffer[Seq[FullOntologyNode]]

      yamlNodes.foreach { yamlNode => // For some really strange reason, map doesn't work here!!!
        if (yamlNode.isInstanceOf[String])
          throw new Exception(s"Ontology has string (${yamlNode.asInstanceOf[String]}) where it should have a map.")
        val map: mutable.Map[String, JCollection[Any]] = yamlNode.asInstanceOf[JMap[String, JCollection[Any]]].asScala
        val keys = map.keys
//        println(s"Keys are $keys")
        val key: String = map.keys.head

        val childNodes = if (key == FullTreeDomainOntology.FIELD)
          Seq(parseOntology(parent, map))
        else {
          // This is to account for leafless branches.
          val yamlNodesOpt = Option(map(key).asScala)
          if (yamlNodesOpt.nonEmpty) { // foreach does not work well here.
            val branchNode = new FullOntologyBranchNode(key, parent, filtered) // Note: branch nodes are created here

            parseOntology(branchNode, yamlNodesOpt.get.toSeq, level + 1)
            Seq(branchNode)
          }
          else
            Seq.empty
        }
        childNodesSeq += childNodes
      }

      val flatChildNodes = childNodesSeq.toSeq.flatten

      if (flatChildNodes.nonEmpty)
        parent.childrenOpt = Some(flatChildNodes)
    }
  }
}
