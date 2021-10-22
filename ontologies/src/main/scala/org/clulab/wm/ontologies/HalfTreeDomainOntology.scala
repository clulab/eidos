package org.clulab.wm.ontologies

import java.time.ZonedDateTime
import java.util.{Collection => JCollection}
import java.util.{Map => JMap}

import org.clulab.utils.Serializer
import org.clulab.wm.eidoscommon.Canonicalizer
import org.clulab.wm.eidoscommon.SentencesExtractor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.Namer
import org.clulab.wm.eidoscommon.utils.Resourcer
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.matching.Regex

@SerialVersionUID(1000L)
abstract class HalfOntologyNode extends DomainOntologyNode with Namer with Serializable {
  // Much of the extra code here is to avoid the root node having a parent of null.

  def parents(parent: HalfOntologyParentNode): Seq[HalfOntologyParentNode] = parent +: parent.parents

  def nodeName: String
  def fullName: String
  def parents: Seq[HalfOntologyParentNode]

  override def toString: String = fullName

  def branch: Option[String]

  def isRoot: Boolean = false

  def isLeaf: Boolean = false

  val name: String = fullName

  def getNamer: Namer = this

  def getValues: Array[String] = Array.empty

  def getPatterns: Option[Array[Regex]] = None
}

@SerialVersionUID(1000L)
abstract class HalfOntologyParentNode extends HalfOntologyNode {
  def isParentRoot: Boolean
}

@SerialVersionUID(1000L)
class HalfOntologyRootNode extends HalfOntologyParentNode {

  override val nodeName: String = ""

  override def fullName: String = nodeName

  override def parents: Seq[HalfOntologyParentNode] = Seq.empty

  def branch: Option[String] = None

  override def isRoot: Boolean = true

  def isParentRoot: Boolean = false

  def getParent: Option[Option[HalfOntologyParentNode]] = Some(None)
}

class HalfOntologyBranchNode(override val nodeName: String, val parent: HalfOntologyParentNode) extends HalfOntologyParentNode {

  override def fullName: String = parent.fullName + DomainOntology.escaped(nodeName) + DomainOntology.SEPARATOR

  // These come out in order parent, grandparent, great grandparent, etc. by design
  override def parents: Seq[HalfOntologyParentNode] = parents(parent)

  override def isRoot: Boolean = false

  def isParentRoot: Boolean = parent.isRoot

  def branch: Option[String] =
      if (parent.isParentRoot) Some(nodeName)
      else parent.branch

  def getParent: Option[Option[HalfOntologyNode]] = Some(Some(parent))
}

@SerialVersionUID(1000L)
class HalfOntologyLeafNode(
  override val nodeName: String,
  val parent: HalfOntologyParentNode,
  polarity: Float,
  /*names: Seq[String],*/
  examples: Option[Array[String]] = None,
  descriptions: Option[Array[String]] = None,
  val patterns: Option[Array[Regex]] = None
) extends HalfOntologyNode with Namer {

  override def fullName: String = parent.fullName + DomainOntology.escaped(nodeName)

  def branch: Option[String] = parent.branch

  // Right now it doesn't matter where these come from, so they can be combined.
  val values: Array[String] = /*names ++*/ examples.getOrElse(Array.empty) ++ descriptions.getOrElse(Array.empty)

  override def toString: String = fullName // + " = " + values.toList

  // These come out in order parent, grandparent, great grandparent, etc. by design
  override def parents: Seq[HalfOntologyParentNode] = parents(parent)

  override def isLeaf: Boolean = true

  def getParent: Option[Option[HalfOntologyNode]] = Some(Some(parent))
}

@SerialVersionUID(1000L)
class HalfTreeDomainOntology(val ontologyNodes: Array[HalfOntologyLeafNode], version: Option[String], date: Option[ZonedDateTime])
    extends VersionedDomainOntology(version, date) with Serializable {

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

    def buildFromYaml(yamlText: String, versionOpt: Option[String] = None, dateOpt: Option[ZonedDateTime] = None): HalfTreeDomainOntology = {
      val yaml = new Yaml(new Constructor(classOf[JCollection[Any]]))
      val yamlNodes = yaml.load(yamlText).asInstanceOf[JCollection[Any]].asScala.toSeq
      val ontologyNodes = parseOntology(new HalfOntologyRootNode, yamlNodes)

      new HalfTreeDomainOntology(ontologyNodes.toArray, versionOpt, dateOpt)
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

      val res = new HalfOntologyLeafNode(name, parent, polarity, /*filteredNames,*/ filteredExamples, filteredDescriptions, patterns)
      res
    }

    protected def parseOntology(parent: HalfOntologyParentNode, yamlNodes: Seq[Any]): Seq[HalfOntologyLeafNode] = {
      yamlNodes.flatMap { yamlNode =>
        if (yamlNode.isInstanceOf[String])
          throw new Exception(s"Ontology has string (${yamlNode.asInstanceOf[String]}) where it should have a map.")
        val map: mutable.Map[String, JCollection[Any]] = yamlNode.asInstanceOf[JMap[String, JCollection[Any]]].asScala
        val key: String = map.keys.head

        if (key == HalfTreeDomainOntology.FIELD)
          Seq(parseOntology(parent, map))
        else {
          // This is to account for leafless branches.
          val yamlNodesOpt = Option(map(key).asScala)
          if (yamlNodesOpt.nonEmpty) // foreach does not work well here.
            parseOntology(new HalfOntologyBranchNode(key, parent), yamlNodesOpt.get.toSeq)
          else
            Seq.empty
        }
      }
    }
  }
}
