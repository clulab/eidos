package org.clulab.wm.ontologies

import java.time.ZonedDateTime
import java.util.{Collection => JCollection}
import java.util.{Map => JMap}
import org.clulab.utils.Serializer
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.SentencesExtractor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.OptionUtils
import org.clulab.wm.eidoscommon.utils.Resourcer
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

@SerialVersionUID(1000L)
abstract class FullOntologyNode(
    val simpleName: String, var parentOpt: Option[FullOntologyParentNode], var childrenOpt: Option[Seq[FullOntologyNode]] = None,
    val values: Option[Array[String]] = None, val patterns: Option[Array[Regex]] = None
) extends DomainOntologyNode with Serializable {
  // At this level there is no distinction made between a parent node and child node.
  // Parent and children are var so that they can be assigned at different times and after object creation.

  def parents(parent: FullOntologyParentNode): Seq[FullOntologyParentNode] = parent +: parent.parents

  def fullName: String
  def parents: Seq[FullOntologyParentNode]

  override def toString: String = fullName

  def isRoot: Boolean = false

  def isLeaf: Boolean = false

  def getName: String = fullName

  override def getSimpleName: String = simpleName

  def getValues: Array[String] = values.getOrElse(Array.empty)

  def getPatterns: Option[Array[Regex]] = patterns

  def getChildren: Seq[FullOntologyNode] = childrenOpt.getOrElse(Seq.empty)

  def getParent: Option[Option[FullOntologyNode]] = Some(this.parentOpt)
}

@SerialVersionUID(1000L)
abstract class FullOntologyParentNode(nodeName: String, parentOpt: Option[FullOntologyParentNode]) extends FullOntologyNode(nodeName, parentOpt) {
  def isParentRoot: Boolean
}

@SerialVersionUID(1000L)
class FullOntologyRootNode extends FullOntologyParentNode("", None) {

  override def fullName: String = ""

  override def parents: Seq[FullOntologyParentNode] = Seq.empty

  override def getBranch: Option[String] = None

  override def isRoot: Boolean = true

  def isParentRoot: Boolean = false
}

class FullOntologyBranchNode(simpleName: String, parent: FullOntologyParentNode, filtered: String => Seq[String])
    extends FullOntologyParentNode(simpleName, Some(parent)) {

  override def fullName: String = parentOpt.get.fullName + DomainOntology.escaped(simpleName) + DomainOntology.SEPARATOR

  // These come out in order parent, grandparent, great grandparent, etc. by design
  override def parents: Seq[FullOntologyParentNode] = parents(parentOpt.get)

  override def isRoot: Boolean = false

  def isParentRoot: Boolean = parent.isRoot

  override def getBranch: Option[String] =
      if (parent.isParentRoot) Some(simpleName)
      else parent.getBranch

  override def getValues: Array[String] = {
    val value = DomainOntology.unescaped(simpleName)
    val values = filtered(value)

    values.toArray
  }
}

@SerialVersionUID(1000L)
class FullOntologyLeafNode(
    simpleName: String,
    val parent: FullOntologyParentNode,
    polarity: Float,
    /*names: Seq[String],*/
    examples: Option[Array[String]] = None,
    descriptions: Option[Array[String]] = None,
    override val patterns: Option[Array[Regex]] = None
) extends FullOntologyNode(simpleName, Some(parent), None, Some(/*names ++*/ examples.getOrElse(Array.empty) ++ descriptions.getOrElse(Array.empty)), patterns) {

  override def fullName: String = parentOpt.get.fullName + DomainOntology.escaped(simpleName)

  override def getBranch: Option[String] = parent.getBranch

  // These come out in order parent, grandparent, great grandparent, etc. by design
  override def parents: Seq[FullOntologyParentNode] = parents(parentOpt.get)

  override def isLeaf: Boolean = true
}

@SerialVersionUID(1000L)
class FullTreeDomainOntology(ontologyNodes: Array[FullOntologyNode], version: Option[String], date: Option[ZonedDateTime])
    extends VersionedDomainOntology(version, date) with Serializable {

  // It is assumed that the root node, for which parentOpt is None, is not in the list.
  def getParents(n: Integer): Seq[FullOntologyParentNode] = ontologyNodes(n).parentOpt.get +: ontologyNodes(n).parentOpt.get.parents

  def save(filename: String): Unit = Serializer.save(this, filename)

  override def nodes: IndexedSeq[FullOntologyNode] = ontologyNodes
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
  class FullTreeDomainOntologyBuilder(sentenceExtractor: SentencesExtractor, canonicalizer: Canonicalizer, filter: Boolean) {

    def buildFromPath(ontologyPath: String, versionOpt: Option[String] = None, dateOpt: Option[ZonedDateTime] = None): FullTreeDomainOntology =
        buildFromYaml(Resourcer.getText(ontologyPath), versionOpt, dateOpt)

    protected def getOntologyNodes(yamlNodes: Seq[Any]): Array[FullOntologyNode] = {
      val rootNode = new FullOntologyRootNode
      val childNodes = parseOntology(rootNode, yamlNodes)

      def walk(node: FullOntologyNode, f: FullOntologyNode => Unit): Unit = {
        node.childrenOpt.foreach { children =>
          children.foreach { child =>
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

      ontologyParentNodes ++ ontologyChildNodes
    }

    def buildFromYaml(yamlText: String, versionOpt: Option[String] = None, dateOpt: Option[ZonedDateTime] = None): FullTreeDomainOntology = {
      val yaml = new Yaml(new Constructor(classOf[JCollection[Any]]))
      val yamlNodes = yaml.load(yamlText).asInstanceOf[JCollection[Any]].asScala.toSeq
      val ontologyNodes = getOntologyNodes(yamlNodes)

      new FullTreeDomainOntology(ontologyNodes, versionOpt, dateOpt)
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
        case Some(regexes) => Some(regexes.map(DomainOntology.toRegex))
        case None => None
      }
    }

    protected def parseOntology(parent: FullOntologyParentNode, yamlNodes: mutable.Map[String, JCollection[Any]]): FullOntologyLeafNode = {
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

      new FullOntologyLeafNode(name, parent, polarity, /*filteredNames,*/ filteredExamples, filteredDescriptions, patterns)
    }

    protected def parseOntology(parent: FullOntologyParentNode, yamlNodes: Seq[Any], level: Int = 0):  Seq[FullOntologyNode] = {
      val childNodes: Seq[FullOntologyNode] = yamlNodes.flatMap { yamlNode => // For some really strange reason, map doesn't work here!!!
        if (yamlNode.isInstanceOf[String])
          throw new Exception(s"Ontology has string (${yamlNode.asInstanceOf[String]}) where it should have a map.")
        val map: mutable.Map[String, JCollection[Any]] = yamlNode.asInstanceOf[JMap[String, JCollection[Any]]].asScala
        val key: String = map.keys.head

        if (key == FullTreeDomainOntology.FIELD)
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
      }

      parent.childrenOpt = OptionUtils.someOrNoneIfEmpty(childNodes)
      childNodes
    }
  }
}
