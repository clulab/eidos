package org.clulab.wm.ontologies

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

import java.time.ZonedDateTime
import java.util.{Collection => JCollection}
import java.util.{Map => JMap}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

@SerialVersionUID(1000L)
abstract class PosNegOntologyNode(
  val simpleName: String, var parentOpt: Option[PosNegOntologyParentNode], var childrenOpt: Option[Seq[PosNegOntologyNode]] = None,
  val posValues: Option[Array[String]] = None, val negValues: Option[Array[String]] = None,
  val posExamplesOpt: Option[Array[String]] = None, val negExamplesOpt: Option[Array[String]] = None,
  val patterns: Option[Array[Regex]] = None
) extends DomainOntologyNode with Serializable {
  // At this level there is no distinction made between a parent node and child node.
  // Parent and children are var so that they can be assigned at different times and after object creation.

  def parents(parent: PosNegOntologyParentNode): Seq[PosNegOntologyParentNode] = parent +: parent.parents

  def fullName: String
  def parents: Seq[PosNegOntologyParentNode]

  override def toString: String = fullName

  def isRoot: Boolean = false

  override def isLeaf: Boolean = false

  def getName: String = fullName

  override def getSimpleName: String = simpleName

  def getValues: Array[String] = getPosValues

  override def getPosValues: Array[String] = posValues.getOrElse(Array.empty)

  override def getNegValues: Array[String] = negValues.getOrElse(Array.empty)

  def getPatternsOpt: Option[Array[Regex]] = patterns

  override def getPosExamplesOpt: Option[Array[String]] = posExamplesOpt

  override def getNegExamplesOpt: Option[Array[String]] = negExamplesOpt

  def getChildren: Seq[PosNegOntologyNode] = childrenOpt.getOrElse(Seq.empty)

  def getParentOptOpt: Option[Option[PosNegOntologyNode]] = Some(parentOpt)

  override def getExamplesOpt: Option[Array[String]] = posExamplesOpt
}

@SerialVersionUID(1000L)
abstract class PosNegOntologyParentNode(
  simpleName: String, parentOpt: Option[PosNegOntologyParentNode],
  posValuesOpt: Option[Array[String]] = None, negValuesOpt: Option[Array[String]] = None,
  posExamplesOpt: Option[Array[String]] = None, negExamplesOpt: Option[Array[String]] = None,
  patternsOpt: Option[Array[Regex]] = None
) extends PosNegOntologyNode(simpleName, parentOpt, None, posValuesOpt, negValuesOpt, posExamplesOpt, negExamplesOpt, patternsOpt) {
  def isParentRoot: Boolean
}

@SerialVersionUID(1000L)
class PosNegOntologyRootNode extends PosNegOntologyParentNode("", None) {

  override def fullName: String = ""

  override def parents: Seq[PosNegOntologyParentNode] = Seq.empty

  override def getBranchOpt: Option[String] = None

  override def isRoot: Boolean = true

  def isParentRoot: Boolean = false
}

class PosNegOntologyBranchNode(simpleName: String, parent: PosNegOntologyParentNode, filtered: String => Seq[String], nodeDataOpt: Option[PosNegTreeDomainOntology.NodeData])
    extends PosNegOntologyParentNode(
      simpleName, Some(parent),
      PosNegOntologyBranchNode.getPosValues(simpleName, nodeDataOpt, filtered),
      PosNegOntologyBranchNode.getNegValues(simpleName, nodeDataOpt, filtered)
    ) {

  override def fullName: String = parentOpt.get.fullName + DomainOntology.escaped(simpleName) + DomainOntology.SEPARATOR

  // These come out in order parent, grandparent, great grandparent, etc. by design
  override def parents: Seq[PosNegOntologyParentNode] = parents(parentOpt.get)

  override def isRoot: Boolean = false

  def isParentRoot: Boolean = parent.isRoot

  override def getBranchOpt: Option[String] =
      if (parent.isParentRoot) Some(simpleName)
      else parent.getBranchOpt
}

object PosNegOntologyBranchNode {

  def getPosValues(simpleName: String, nodeDataOpt: Option[PosNegTreeDomainOntology.NodeData], filtered: String => Seq[String]): Option[Array[String]] = {
    val value = DomainOntology.unescaped(simpleName)
    val filteredValues = filtered(value).toArray
    val posFilteredExamples = nodeDataOpt.flatMap(_.posExamplesOpt.map(_.flatMap(filtered))).getOrElse(Array.empty)
    val filteredDescriptions = nodeDataOpt.flatMap(_.descriptionsOpt.map(_.flatMap(filtered))).getOrElse(Array.empty)

    Some(filteredValues ++ posFilteredExamples ++ filteredDescriptions)
  }

  def getNegValues(nodeName: String, nodeDataOpt: Option[PosNegTreeDomainOntology.NodeData], filtered: String => Seq[String]): Option[Array[String]] = {
    val negFilteredExamples = nodeDataOpt.flatMap(_.negExamplesOpt.map(_.flatMap(filtered)))
    negFilteredExamples
  }
}

@SerialVersionUID(1000L)
class PosNegOntologyLeafNode(
  simpleName: String,
  val parent: PosNegOntologyParentNode,
  polarity: Float,
  /*names: Seq[String],*/
  posExamplesOpt: Option[Array[String]] = None,
  negExamplesOpt: Option[Array[String]] = None,
  descriptions: Option[Array[String]] = None,
  override val patterns: Option[Array[Regex]] = None,
  rawPosExamplesOpt: Option[Array[String]] = None,
  rawNegExamplesOpt: Option[Array[String]] = None
) extends PosNegOntologyNode(simpleName, Some(parent), None,
    Some(/*names ++*/ posExamplesOpt.getOrElse(Array.empty) ++ descriptions.getOrElse(Array.empty)),
    Some(negExamplesOpt.getOrElse(Array.empty)),
    rawPosExamplesOpt, rawNegExamplesOpt, patterns) {

  override def fullName: String = parentOpt.get.fullName + DomainOntology.escaped(simpleName)

  override def getBranchOpt: Option[String] = parent.getBranchOpt

  // These come out in order parent, grandparent, great grandparent, etc. by design
  override def parents: Seq[PosNegOntologyParentNode] = parents(parentOpt.get)

  override def isLeaf: Boolean = true
}

@SerialVersionUID(1000L)
class PosNegTreeDomainOntology(val ontologyNodes: Array[PosNegOntologyNode], version: Option[String], date: Option[ZonedDateTime])
    extends VersionedDomainOntology(version, date) with Serializable {

  def getPosValues(n: Integer): Array[String] = ontologyNodes(n).getPosValues

  def getNegValues(n: Integer): Array[String] = ontologyNodes(n).getNegValues

  // It is assumed that the root node, for which parentOpt is None, is not in the list.
  def getParents(n: Integer): Seq[PosNegOntologyParentNode] = ontologyNodes(n).parentOpt.get +: ontologyNodes(n).parentOpt.get.parents

  def save(filename: String): Unit = Serializer.save(this, filename)

  override def nodes: IndexedSeq[DomainOntologyNode] = ontologyNodes
}

object PosNegTreeDomainOntology {

  case class NodeData(name: String, posExamplesOpt: Option[Array[String]], negExamplesOpt: Option[Array[String]], descriptionsOpt: Option[Array[String]], patterns: Option[Array[Regex]], polarity: Float)

  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val FIELD = "OntologyNode"
  val INNER_FIELD = "InnerOntologyNode"
  val NAME = "name"
  val POS_EXAMPLES = "examples"
  val NEG_EXAMPLES = "neg_examples"
  val DESCRIPTION = "descriptions"
  val POLARITY = "polarity"
  val PATTERN = "pattern"

  def isPosNegName(name: String): Boolean = name.contains("posneg")

  def isPosNegPath(name: String): Boolean = name.contains("posneg")

  def load(path: String): PosNegTreeDomainOntology = {
    logger.info(s"Loading serialized Ontology from $path")
    val domainOntology = FileUtils.load[PosNegTreeDomainOntology](path, this)
    logger.info("Serialized Ontology successfully loaded.")
    domainOntology
  }

  // This is mostly here to capture sentenceExtractor so that it doesn't have to be passed around.
  class PosNegTreeDomainOntologyBuilder(sentenceExtractor: SentencesExtractor, canonicalizer: Canonicalizer, filter: Boolean) {

    def buildFromPath(ontologyPath: String, versionOpt: Option[String] = None, dateOpt: Option[ZonedDateTime] = None): PosNegTreeDomainOntology =
        buildFromYaml(Resourcer.getText(ontologyPath), versionOpt, dateOpt)

    protected def getOntologyNodes(yamlNodes: Seq[Any]): Array[PosNegOntologyNode] = {
      val rootNode = new PosNegOntologyRootNode
      val childNodes = parseOntology(rootNode, yamlNodes)

      def walk(node: PosNegOntologyNode, f: PosNegOntologyNode => Unit): Unit = {
        node.childrenOpt.foreach { children =>
          children.foreach { child =>
            f(child)
            walk(child, f)
          }
        }
      }

      val (ontologyParentNodes, ontologyChildNodes) = {
        val parents = new ArrayBuffer[PosNegOntologyNode]
        val children = new ArrayBuffer[PosNegOntologyNode]

        walk(rootNode, { node: PosNegOntologyNode =>
          if (node.isLeaf) children.append(node)
          else if (!node.parentOpt.get.isInstanceOf[PosNegOntologyRootNode])
            parents.append(node)
        })

        (parents.toArray, children.toArray)
      }

      ontologyParentNodes ++ ontologyChildNodes
    }

    def buildFromYaml(yamlText: String, versionOpt: Option[String] = None, dateOpt: Option[ZonedDateTime] = None): PosNegTreeDomainOntology = {
      val yaml = new Yaml(new Constructor(classOf[JCollection[Any]]))
      val yamlNodes = yaml.load(yamlText).asInstanceOf[JCollection[Any]].asScala.toSeq
      val ontologyNodes = getOntologyNodes(yamlNodes)

      new PosNegTreeDomainOntology(ontologyNodes, versionOpt, dateOpt)
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

    protected def parseNodeData(name: String, yamlNodes: mutable.Map[String, JCollection[Any]]): NodeData = {
      /*val names = (name +: parent.nodeName +: parent.parents.map(_.nodeName)).map(unescape)*/
      val posExamples = yamlNodesToStrings(yamlNodes, PosNegTreeDomainOntology.POS_EXAMPLES)
          .map { examples => examples.filter(_ != null) }
      val negExamples = yamlNodesToStrings(yamlNodes, PosNegTreeDomainOntology.NEG_EXAMPLES)
          .map { examples => examples.filter(_ != null) }
      val descriptions: Option[Array[String]] = yamlNodesToStrings(yamlNodes, PosNegTreeDomainOntology.DESCRIPTION)
      // The incoming polarity can now be Int or Double.  We will store either one as a Float.
      val polarity = {
        // There's something wrong with this type system, obviously.  This is legacy code.
        val yamlNodesOpt: Option[JCollection[Any]] = yamlNodes.get(PosNegTreeDomainOntology.POLARITY)

        yamlNodesOpt.map { yamlNode: Any =>
          yamlNode match {
            case value: Double => value.toFloat
            case value: Int => value.toFloat
            case _ => throw new Exception(s"Unexpected polarity value: $yamlNode!")
          }
        }.getOrElse(1.0f) // positive by default
      }
      val patterns: Option[Array[Regex]] = yamlNodesToRegexes(yamlNodes, PosNegTreeDomainOntology.PATTERN)

      NodeData(name, posExamples, negExamples, descriptions, patterns, polarity)
    }

    protected def parseNodeData(yamlNodes: mutable.Map[String, JCollection[Any]]): NodeData = {
      val name = yamlNodes(PosNegTreeDomainOntology.NAME).asInstanceOf[String]
      parseNodeData(name, yamlNodes)
    }

    protected def parseOntology(parent: PosNegOntologyParentNode, yamlNodes: mutable.Map[String, JCollection[Any]]): PosNegOntologyLeafNode = {
      val nodeData = parseNodeData(yamlNodes)

      /*val filteredNames = names.flatMap(filtered)*/
      val posFilteredExamples = nodeData.posExamplesOpt.map(_.flatMap(filtered)) // Note: These have all been filtered!
      val negFilteredExamples = nodeData.negExamplesOpt.map(_.flatMap(filtered))
      val filteredDescriptions = nodeData.descriptionsOpt.map(_.flatMap(filtered))

      new PosNegOntologyLeafNode(nodeData.name, parent, nodeData.polarity, /*filteredNames,*/ 
          posFilteredExamples, negFilteredExamples, filteredDescriptions, nodeData.patterns,
          nodeData.posExamplesOpt, nodeData.negExamplesOpt)
    }

    protected def parseOntology(parent: PosNegOntologyParentNode, yamlNodes: Seq[Any], level: Int = 0): Seq[PosNegOntologyNode] = {
      val childNodes: Seq[PosNegOntologyNode] = yamlNodes.flatMap { yamlNode => // For some really strange reason, map doesn't work here!!!
        if (yamlNode.isInstanceOf[String])
          throw new Exception(s"Ontology has string (${yamlNode.asInstanceOf[String]}) where it should have a map.")
        val map: mutable.Map[String, JCollection[Any]] = yamlNode.asInstanceOf[JMap[String, JCollection[Any]]].asScala
        val key = map.keys.head
        val isLeaf = key == PosNegTreeDomainOntology.FIELD

        if (isLeaf)
          Seq(parseOntology(parent, map))
        else {
          // This is to account for leafless branches.
          val yamlNodesOpt = Option(map(key).asScala)
          if (yamlNodesOpt.nonEmpty) { // foreach does not work well here.
            val yamlNodes = yamlNodesOpt.get.toSeq
            val yamlObjects = yamlNodes.map(_.asInstanceOf[JMap[String, JCollection[Any]]].asScala)
            val hasData = yamlObjects.head.isDefinedAt(PosNegTreeDomainOntology.INNER_FIELD)
            val branchNode =
              if (!hasData) {
                val branchNode = new PosNegOntologyBranchNode(key, parent, filtered, None)
                parseOntology(branchNode, yamlNodes, level + 1)
                branchNode
              }
              else {
                val nodeData = parseNodeData(key, yamlObjects.head)
                val branchNode = new PosNegOntologyBranchNode(key, parent, filtered, Some(nodeData))
                parseOntology(branchNode, yamlNodes.tail, level + 1)
                branchNode
              }

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

  def main(args: Array[String]): Unit = {
    val builder = new PosNegTreeDomainOntologyBuilder(null, null, false)
    val ontology = builder.buildFromPath("/org/clulab/wm/eidos/english/ontologies/wm_posneg_metadata.yml")

    ontology.nodes.foreach { node =>
      val name = node.getName
      val negValues = node.getNegValues

      if (negValues.nonEmpty)
        println(name)
    }
  }
}
