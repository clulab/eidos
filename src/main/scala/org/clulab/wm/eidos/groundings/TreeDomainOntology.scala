package org.clulab.wm.eidos.groundings

import java.util.{Collection => JCollection, Map => JMap}

import org.clulab.processors.{Processor, Sentence}
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.utils.Serializer
import org.clulab.wm.eidos.utils.FileUtils.getTextFromResource
import org.clulab.wm.eidos.utils.Namer
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.collection.JavaConverters._
import scala.collection.mutable

@SerialVersionUID(1000L)
class OntologyNode(var nodeName: String, var parent: OntologyBranchNode) extends Serializable {

  // There can already be a / in any of the stages of the route that must be escaped.
  // First, double up any existing backslashes, then escape the forward slashes with backslashes.
  def escaped: String =
      nodeName
          .replace(DomainOntology.ESCAPE, DomainOntology.ESCAPED_ESCAPE)
          .replace(DomainOntology.SEPARATOR, DomainOntology.ESCAPED_SEPARATOR)

  def fullName: String =
      if (parent != null) parent.fullName + DomainOntology.SEPARATOR + escaped
      else escaped

  override def toString = fullName
}

@SerialVersionUID(1000L)
class OntologyBranchNode(nodeName: String, parent: OntologyBranchNode) extends OntologyNode(nodeName, parent) {

  // These come out in order parent, grandparent, great grandparent, etc. by design
  def parents: Seq[OntologyBranchNode] =
      if (parent == null) Seq.empty
      else parent +: parent.parents
}

@SerialVersionUID(1000L)
class OntologyLeafNode(nodeName: String, parent: OntologyBranchNode, polarity: Double, examples: Option[Seq[String]] = None, descriptions: Option[Seq[String]] = None) extends OntologyNode(nodeName, parent) with Namer {

  def name: String = fullName

  protected def split(values: Option[Seq[String]]): Seq[String] =
      if (values.isEmpty) Seq.empty
      else values.get.flatMap(_.split(" +"))

  // Right now it doesn't matter where these come from, so they can be combined.
  val values: Array[String] = (split(examples) ++ split(descriptions)).toArray

  override def toString = super.fullName + " = " + values.toList
}

@SerialVersionUID(1000L)
class TreeDomainOntology(val name: String, val ontologyNodes: Array[OntologyLeafNode]) extends DomainOntology with Serializable {

  def size: Integer = ontologyNodes.size

  def getNamer(n: Integer): Namer = ontologyNodes(n)

  def getValues(n: Integer): Array[String] = ontologyNodes(n).values

  def getNode(n: Integer): OntologyLeafNode = ontologyNodes(n)

  def getParents(n: Integer): Seq[OntologyBranchNode] = ontologyNodes(n).parent +: ontologyNodes(n).parent.parents

  def save(filename: String) = {
    Serializer.save(this, filename)
  }
}

object TreeDomainOntology {
  val FIELD = "OntologyNode"
  val NAME = "name"
  val EXAMPLES = "examples"
  val DESCRIPTION = "descriptions"
  val POLARITY = "polarity"

  def load(path: String): TreeDomainOntology = DomainOntology.updatedLoad[TreeDomainOntology](path)

  // This is mostly here to capture proc so that it doesn't have to be passed around.
  class TreeDomainOntologyBuilder(name: String, ontologyPath: String, cachedDir: String, proc: Processor, filter: Boolean) {

    def build(): TreeDomainOntology = {
      val text = getTextFromResource(ontologyPath)
      val yaml = new Yaml(new Constructor(classOf[JCollection[Any]]))
      val yamlNodes = yaml.load(text).asInstanceOf[JCollection[Any]].asScala.toSeq
      val ontologyNodes = parseOntology(null, yamlNodes)

      new TreeDomainOntology(name, ontologyNodes.toArray)
    }

    protected val getSentences: (String => Array[Sentence]) = proc match {
      // Earlier, a complete annotation was performed.
      // val sentences = proc.annotate(text).sentences
      // Now we just go through the POS tagging stage, but the procedure is
      // different for different kinds of processors.
      case proc: CluProcessor => (text => {
        val doc = proc.mkDocument(text)

        // This is the key difference.  Lemmatization must happen first.
        proc.lemmatize(doc)
        proc.tagPartsOfSpeech(doc)
        doc.sentences
      })
      case proc: ShallowNLPProcessor => (text => {
        val doc = proc.mkDocument(text)

        if (doc.sentences.nonEmpty)
          proc.tagPartsOfSpeech(doc)
        // Lemmatization, if needed, would happen afterwards.
        doc.sentences
      })
    }

    protected def realFiltered(text: String): Seq[String] = {
      val sentences = getSentences(text)

      sentences.flatMap { sentence =>
        sentence.words.zip(sentence.tags.get).filter { wordAndPos =>
          // Filter by POS tags which need to be kept (Nouns, Adjectives, and Verbs).
          wordAndPos._2.contains("NN") ||
            wordAndPos._2.contains("JJ") ||
            wordAndPos._2.contains("VB")
        }.map(_._1) // Get only the words.
      }
    }

    protected def fakeFiltered(text: String): Seq[String] = text.split(" +")

    protected val filtered: String => Seq[String] = if (filter) realFiltered else fakeFiltered

    protected def yamlNodesToStrings(yamlNodes: mutable.Map[String, JCollection[Any]], name: String): Option[Seq[String]] =
      yamlNodes.get(name).map(_.asInstanceOf[JCollection[String]].asScala.toSeq)

    protected def parseOntology(parent: OntologyBranchNode, yamlNodes: mutable.Map[String, JCollection[Any]]): OntologyLeafNode = {
      val name = yamlNodes.get(TreeDomainOntology.NAME).get.asInstanceOf[String]
      val examples = yamlNodesToStrings(yamlNodes, TreeDomainOntology.EXAMPLES)
      val descriptions = yamlNodesToStrings(yamlNodes, TreeDomainOntology.DESCRIPTION)
      val polarity = yamlNodes.getOrElse(TreeDomainOntology.POLARITY, 1.0).asInstanceOf[Double]
      val filteredDescriptions =
          if (descriptions.isEmpty) descriptions
          else Some(descriptions.get.flatMap(filtered))

      new OntologyLeafNode(name, parent, polarity,  examples, filteredDescriptions)
    }

    protected def parseOntology(parent: OntologyBranchNode, yamlNodes: Seq[Any]): Seq[OntologyLeafNode] = {
      yamlNodes flatMap { yamlNode =>
        if (yamlNode.isInstanceOf[String])
          throw new Exception(s"Ontology has string (${yamlNode.asInstanceOf[String]}) where it should have a map.")
        val map: mutable.Map[String, JCollection[Any]] = yamlNode.asInstanceOf[JMap[String, JCollection[Any]]].asScala
        val key: String = map.keys.head

        if (key == TreeDomainOntology.FIELD)
          Seq(parseOntology(parent, map))
        else
          parseOntology(new OntologyBranchNode(key, parent), map(key).asScala.toSeq)
      }
    }
  }
}
