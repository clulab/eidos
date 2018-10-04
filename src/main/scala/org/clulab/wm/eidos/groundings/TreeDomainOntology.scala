package org.clulab.wm.eidos.groundings

import java.util.{Collection => JCollection, Map => JMap}

import org.clulab.processors.{Processor, Sentence}
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.utils.Serializer
import org.clulab.wm.eidos.utils.FileUtils.getTextFromResource
import org.clulab.wm.eidos.utils.{Canonicalizer, Namer}
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

  override def toString(): String = fullName
}

@SerialVersionUID(1000L)
class OntologyBranchNode(nodeName: String, parent: OntologyBranchNode) extends OntologyNode(nodeName, parent) {

  // These come out in order parent, grandparent, great grandparent, etc. by design
  def parents: Seq[OntologyBranchNode] =
      if (parent == null) Seq.empty
      else parent +: parent.parents
}

@SerialVersionUID(1000L)
class OntologyLeafNode(nodeName: String, parent: OntologyBranchNode, polarity: Float, /*names: Seq[String],*/ examples: Option[Seq[String]] = None, descriptions: Option[Seq[String]] = None) extends OntologyNode(nodeName, parent) with Namer {

  def name: String = fullName

  // Right now it doesn't matter where these come from, so they can be combined.
  val values: Array[String] = (/*names ++*/ examples.getOrElse(Seq.empty) ++ descriptions.getOrElse(Seq.empty)).toArray

  override def toString(): String = super.fullName + " = " + values.toList
}

@SerialVersionUID(1000L)
class TreeDomainOntology(val ontologyNodes: Array[OntologyLeafNode]) extends DomainOntology with Serializable {

  def size: Integer = ontologyNodes.length

  def getNamer(n: Integer): Namer = ontologyNodes(n)

  def getValues(n: Integer): Array[String] = ontologyNodes(n).values

  def getNode(n: Integer): OntologyLeafNode = ontologyNodes(n)

  def getParents(n: Integer): Seq[OntologyBranchNode] = ontologyNodes(n).parent +: ontologyNodes(n).parent.parents

  def save(filename: String): Unit = {
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
  class TreeDomainOntologyBuilder(ontologyPath: String, proc: Processor, canonicalizer: Canonicalizer, filter: Boolean) {

    def build(): TreeDomainOntology = {
      val text = getTextFromResource(ontologyPath)
      val yaml = new Yaml(new Constructor(classOf[JCollection[Any]]))
      val yamlNodes = yaml.load(text).asInstanceOf[JCollection[Any]].asScala.toSeq
      val ontologyNodes = parseOntology(null, yamlNodes)

      new TreeDomainOntology(ontologyNodes.toArray)
    }

    protected val getSentences: String => Array[Sentence] = proc match {
      // Earlier, a complete annotation was performed.
      // val sentences = proc.annotate(text).sentences
      // Now we just go through the POS tagging stage, but the procedure is
      // different for different kinds of processors.
      case proc: CluProcessor => text => {
        val doc = proc.mkDocument(text)

        // This is the key difference.  Lemmatization must happen first.
        proc.lemmatize(doc)
        proc.tagPartsOfSpeech(doc)
        proc.recognizeNamedEntities(doc)
        doc.sentences
      }
      case proc: ShallowNLPProcessor => text => {
        val doc = proc.mkDocument(text)

        if (doc.sentences.nonEmpty) {
          proc.tagPartsOfSpeech(doc)
          proc.lemmatize(doc)
          proc.recognizeNamedEntities(doc)
        }
        doc.sentences
      }
    }

    protected def realFiltered(text: String): Seq[String] = {
      val result = getSentences(text).flatMap { sentence =>
        val lemmas: Array[String] = sentence.lemmas.get
        val tags: Array[String] = sentence.tags.get
        val ners: Array[String] = sentence.entities.get

        for {
          i <- lemmas.indices
          if canonicalizer.isCanonical(lemmas(i), tags(i), ners(i))
        } yield lemmas(i)
      }
      result // breakpoint
    }

    protected def fakeFiltered(text: String): Seq[String] = text.split(" +")

    protected val filtered: String => Seq[String] = if (filter) realFiltered else fakeFiltered

    protected def yamlNodesToStrings(yamlNodes: mutable.Map[String, JCollection[Any]], name: String): Option[Seq[String]] =
      yamlNodes.get(name).map(_.asInstanceOf[JCollection[String]].asScala.toSeq)

    protected def unescape(name: String): String = {
      // Sometimes the words in names are concatenated with _
      // TODO: We should avoid this practice
      name.replace('_', ' ')
    }

    protected def parseOntology(parent: OntologyBranchNode, yamlNodes: mutable.Map[String, JCollection[Any]]): OntologyLeafNode = {
      /* We're going without the names for now. */
      val name = yamlNodes(TreeDomainOntology.NAME).asInstanceOf[String]
      /*val names = (name +: parent.nodeName +: parent.parents.map(_.nodeName)).map(unescape)*/
      val examples = yamlNodesToStrings(yamlNodes, TreeDomainOntology.EXAMPLES)
      val descriptions: Option[Seq[String]] = yamlNodesToStrings(yamlNodes, TreeDomainOntology.DESCRIPTION)
      val polarity = yamlNodes.getOrElse(TreeDomainOntology.POLARITY, 1.0d).asInstanceOf[Double].toFloat

      /*val filteredNames = names.flatMap(filtered)*/
      val filteredExamples = examples.map(_.flatMap(filtered))
      val filteredDescriptions = descriptions.map(_.flatMap(filtered))

      val res = new OntologyLeafNode(name, parent, polarity, /*filteredNames,*/ filteredExamples, filteredDescriptions)
      res
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
