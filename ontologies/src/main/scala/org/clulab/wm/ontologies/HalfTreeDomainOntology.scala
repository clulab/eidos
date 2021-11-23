package org.clulab.wm.ontologies

import org.clulab.utils.Serializer
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.SentencesExtractor
import org.clulab.wm.eidoscommon.utils.FileUtils
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
import scala.util.matching.Regex

@SerialVersionUID(1000L)
abstract class HalfOntologyNode extends DomainOntologyNode with Serializable {
  // Much of the extra code here is to avoid the root node having a parent of null.

  def parents(parent: HalfOntologyParentNode): Seq[HalfOntologyParentNode] = parent +: parent.parents

  def fullName: String
  def parents: Seq[HalfOntologyParentNode]

  override def toString: String = fullName

  def isRoot: Boolean = false

  def isLeaf: Boolean = false

  def getName: String = fullName

  def getValues: Array[String] = Array.empty

  def getPatternsOpt: Option[Array[Regex]] = None
}

@SerialVersionUID(1000L)
abstract class HalfOntologyParentNode extends HalfOntologyNode {
  def isParentRoot: Boolean
}

@SerialVersionUID(1000L)
class HalfOntologyRootNode extends HalfOntologyParentNode {

  override def fullName: String = ""

  override def getSimpleName: String = ""

  override def parents: Seq[HalfOntologyParentNode] = Seq.empty

  override def getBranch: Option[String] = None

  override def isRoot: Boolean = true

  def isParentRoot: Boolean = false

  def getParent: Option[Option[HalfOntologyParentNode]] = Some(None)
}

class HalfOntologyBranchNode(val simpleName: String, val parent: HalfOntologyParentNode) extends HalfOntologyParentNode {

  override def fullName: String = parent.fullName + DomainOntology.escaped(simpleName) + DomainOntology.SEPARATOR

  // These come out in order parent, grandparent, great grandparent, etc. by design
  override def parents: Seq[HalfOntologyParentNode] = parents(parent)

  override def isRoot: Boolean = false

  def isParentRoot: Boolean = parent.isRoot

  override def getBranch: Option[String] =
      if (parent.isParentRoot) Some(simpleName)
      else parent.getBranch

  def getParent: Option[Option[HalfOntologyNode]] = Some(Some(parent))

  override def getSimpleName: String = simpleName
}

@SerialVersionUID(1000L)
class HalfOntologyLeafNode(
  val simpleName: String,
  val parent: HalfOntologyParentNode,
  polarity: Float,
  /*names: Seq[String],*/
  examples: Option[Array[String]] = None,
  descriptions: Option[Array[String]] = None,
  val patterns: Option[Array[Regex]] = None
) extends HalfOntologyNode {

  override def fullName: String = parent.fullName + DomainOntology.escaped(simpleName)

  override def getBranch: Option[String] = parent.getBranch

  // Right now it doesn't matter where these come from, so they can be combined.
  val values: Array[String] = /*names ++*/ examples.getOrElse(Array.empty) ++ descriptions.getOrElse(Array.empty)

  override def toString: String = fullName // + " = " + values.toList

  // These come out in order parent, grandparent, great grandparent, etc. by design
  override def parents: Seq[HalfOntologyParentNode] = parents(parent)

  override def isLeaf: Boolean = true

  def getParent: Option[Option[HalfOntologyNode]] = Some(Some(parent))

  override def getSimpleName: String = simpleName
}

@SerialVersionUID(1000L)
class HalfTreeDomainOntology(val ontologyNodes: Array[HalfOntologyLeafNode], version: Option[String], date: Option[ZonedDateTime])
    extends VersionedDomainOntology(version, date) with Serializable {

  // It is assumed that the root node, for which parentOpt is None, is not in the list.
  def getParents(n: Integer): Seq[HalfOntologyParentNode] = ontologyNodes(n).parent +: ontologyNodes(n).parent.parents

  def save(filename: String): Unit = Serializer.save(this, filename)

  override def nodes: IndexedSeq[HalfOntologyNode] = ontologyNodes
}

object HalfTreeDomainOntology {
  protected lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val FIELD = "OntologyNode"
  val NAME = "name"
  val EXAMPLES = "examples"
  val DESCRIPTION = "descriptions"
  val POLARITY = "polarity"
  val PATTERN = "pattern"

  def load(path: String): HalfTreeDomainOntology = {
    logger.info(s"Loading serialized Ontology from $path")
    val domainOntology = FileUtils.load[HalfTreeDomainOntology](path, this)
    logger.info("Serialized Ontology successfully loaded.")
    domainOntology
  }

  // This is mostly here to capture sentenceExtractor so that it doesn't have to be passed around.
  class HalfTreeDomainOntologyBuilder(sentenceExtractor: SentencesExtractor, canonicalizer: Canonicalizer, filter: Boolean) {

    def buildFromPath(ontologyPath: String, versionOpt: Option[String] = None, dateOpt: Option[ZonedDateTime] = None): HalfTreeDomainOntology =
        buildFromYaml(Resourcer.getText(ontologyPath), versionOpt, dateOpt)

    protected def getOntologyNodes(yamlNodes: Seq[Any]): Array[HalfOntologyLeafNode] = {
      val rootNode = new HalfOntologyRootNode
      val childNodes = parseOntology(rootNode, yamlNodes).toArray

      childNodes
    }

    def buildFromYaml(yamlText: String, versionOpt: Option[String] = None, dateOpt: Option[ZonedDateTime] = None): HalfTreeDomainOntology = {
      val yaml = new Yaml(new Constructor(classOf[JCollection[Any]]))
      val yamlNodes = yaml.load(yamlText).asInstanceOf[JCollection[Any]].asScala.toSeq
      val ontologyNodes = getOntologyNodes(yamlNodes)

      new HalfTreeDomainOntology(ontologyNodes, versionOpt, dateOpt)
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

    protected def parseOntology(parent: HalfOntologyParentNode, yamlNodes: mutable.Map[String, JCollection[Any]]): HalfOntologyLeafNode = {
      /* We're going without the names for now. */
      val name = yamlNodes(HalfTreeDomainOntology.NAME).asInstanceOf[String]
      /*val names = (name +: parent.nodeName +: parent.parents.map(_.nodeName)).map(unescape)*/
      val examples = yamlNodesToStrings(yamlNodes, HalfTreeDomainOntology.EXAMPLES)
          .map { examples => examples.filter(_ != null) }
      val descriptions: Option[Array[String]] = yamlNodesToStrings(yamlNodes, HalfTreeDomainOntology.DESCRIPTION)
      // The incoming polarity can now be Int or Double.  We will store either one as a Float.
      val polarity = {
        // There's something wrong with this type system, obviously.  This is legacy code.
        val yamlNodesOpt: Option[JCollection[Any]] = yamlNodes.get(HalfTreeDomainOntology.POLARITY)

        yamlNodesOpt.map { yamlNode: Any =>
          yamlNode match {
            case value: Double => value.toFloat
            case value: Int => value.toFloat
            case _ => throw new Exception(s"Unexpected polarity value: $yamlNode!")
          }
        }.getOrElse(1.0f) // positive by default
      }
      val patterns: Option[Array[Regex]] = yamlNodesToRegexes(yamlNodes, HalfTreeDomainOntology.PATTERN)

      /*val filteredNames = names.flatMap(filtered)*/
      val filteredExamples = examples.map(_.flatMap(filtered))
      val filteredDescriptions = descriptions.map(_.flatMap(filtered))

      new HalfOntologyLeafNode(name, parent, polarity, /*filteredNames,*/ filteredExamples, filteredDescriptions, patterns)
    }

    protected def parseOntology(parent: HalfOntologyParentNode, yamlNodes: Seq[Any]): Seq[HalfOntologyLeafNode] = {
      val childNodes = yamlNodes.flatMap { yamlNode =>
        if (yamlNode.isInstanceOf[String])
          throw new Exception(s"Ontology has string (${yamlNode.asInstanceOf[String]}) where it should have a map.")
        val map: mutable.Map[String, JCollection[Any]] = yamlNode.asInstanceOf[JMap[String, JCollection[Any]]].asScala
        val key = map.keys.head
        val isLeaf = key == HalfTreeDomainOntology.FIELD

        if (isLeaf)
          Seq(parseOntology(parent, map))
        else {
          // This is to account for leafless branches.
          val yamlNodesOpt = Option(map(key).asScala)
          if (yamlNodesOpt.nonEmpty) { // foreach does not work well here.
            val branchNode = new HalfOntologyBranchNode(key, parent)

            parseOntology(branchNode, yamlNodesOpt.get.toSeq)
          }
          else
            Seq.empty
        }
      }

      childNodes
    }
  }
}
