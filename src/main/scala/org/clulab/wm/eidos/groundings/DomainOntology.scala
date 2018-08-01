package org.clulab.wm.eidos.groundings

import java.io.{FileInputStream, ObjectInputStream}
import java.util.{Collection => JCollection, Map => JMap}

import org.clulab.utils.{ClassLoaderObjectInputStream, Serializer}
import org.clulab.processors.{Processor, Sentence}
import org.clulab.processors.clu.CluProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.wm.eidos.utils.FileUtils.getTextFromResource
import org.slf4j.LoggerFactory
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.collection.JavaConverters._
import scala.collection.mutable

@SerialVersionUID(1000L)
class OntologyNode(crumbs: Seq[String], name: String, polarity: Double, examples: Option[Seq[String]] = None, descriptions: Option[Seq[String]] = None) extends Serializable {

  protected def split(values: Option[Seq[String]]): Seq[String] =
      if (values.isEmpty) Seq.empty
      else values.get.flatMap(_.split(" +"))

  // There can already be a / in any of the stages of the route that must be escaped.
  // First, double up any existing backslashes, then escape the forward slashes with backslashes.
  protected def escapeRoute: Seq[String] =
      route.map(_.replace("\\", "\\\\").replace(OntologyNode.SEPARATOR, "\\" + OntologyNode.SEPARATOR))

  def path: String = escapeRoute.mkString(OntologyNode.SEPARATOR)

  val route = (name +: crumbs).reverse.toArray

  // Right now it doesn't matter where these come from, so they can be combined.
  val values: Array[String] = (split(examples) ++ split(descriptions)).toArray

  override def toString = path + " = " + values.toList
}

object OntologyNode {
  val SEPARATOR = "/"
}

@SerialVersionUID(1000L)
class DomainOntology(val name: String, val ontologyNodes: Array[OntologyNode]) extends Serializable {

  def iterateOntology(wordToVec: EidosWordToVec): Seq[ConceptEmbedding] =
      ontologyNodes.map { ontologyNode =>
        ConceptEmbedding(ontologyNode.path, wordToVec.makeCompositeVector(ontologyNode.values))
      }

  def save(filename: String) = {
    Serializer.save(this, filename)
  }
}

object DomainOntology {
  val FIELD = "OntologyNode"
  val NAME = "name"
  val EXAMPLES = "examples"
  val DESCRIPTION = "descriptions"
  val POLARITY = "polarity"

  val logger = LoggerFactory.getLogger(this.getClass())

  def serializedPath(name: String, dir: String): String = s"$dir/$name.serialized"

  // fixme: the following lines are almost directly from Serializer.  We were having problems with the ClassLoader not
  // being able to find DomainOntology from within the webapp submodule, so we put it here.  This is not ideal
  // and should be fixed, probably in Processors.
  def updatedLoad[A](filename: String, classProvider: Any = this): A = {
    val classLoader = classProvider.getClass().getClassLoader()
    val fileInputStream = new FileInputStream(filename)
    var objectInputStream: ObjectInputStream = null

    try {
      objectInputStream = new ClassLoaderObjectInputStream(classLoader, fileInputStream)

      objectInputStream.readObject().asInstanceOf[A]
    }
    finally {
      if (objectInputStream != null)
        objectInputStream.close()
      else
        fileInputStream.close()
    }
  }

  // Load from serialized
  def load(path: String): DomainOntology = {
    logger.info(s"Loading serialized ontology from $path")
    val obj = updatedLoad[DomainOntology](path)
    logger.info("Serialized Ontology successfully loaded.")
    obj
  }

  // This is mostly here to capture proc so that it doesn't have to be passed around.
  class DomainOntologyBuilder(name: String, ontologyPath: String, cachedDir: String, proc: Processor, filter: Boolean) {

    def build(): DomainOntology = {
      val text = getTextFromResource(ontologyPath)
      val yaml = new Yaml(new Constructor(classOf[JCollection[Any]]))
      val yamlNodes = yaml.load(text).asInstanceOf[JCollection[Any]].asScala.toSeq
      val ontologyNodes = parseOntology(yamlNodes, Seq.empty, Seq.empty)

      new DomainOntology(name, ontologyNodes.toArray)
    }

    protected val getSentences: (String => Array[Sentence]) = proc match {
      // Earlier, a complete annotation was performed.
      // val sentences = proc.annotate(text).sentences
      // Now we just go through the POS tagging stage, but the procedure is
      // different for different kinds of processors.
      case proc: CluProcessor => (text => {
        val doc = proc.mkDocument(proc.preprocessText(text))

        // This is the key difference.  Lemmatization must happen first.
        proc.lemmatize(doc)
        proc.tagPartsOfSpeech(doc)
        doc.sentences
      })
      case proc: ShallowNLPProcessor => (text => {
        val doc = proc.mkDocument(proc.preprocessText(text))

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

    protected def parseOntology(yamlNodes: mutable.Map[String, JCollection[Any]], ontologyNodes: Seq[OntologyNode],
        route: Seq[String]): Seq[OntologyNode] = {
      val name = yamlNodes.get(DomainOntology.NAME).get.asInstanceOf[String]
      val examples = yamlNodesToStrings(yamlNodes, DomainOntology.EXAMPLES)
      val descriptions = yamlNodesToStrings(yamlNodes, DomainOntology.DESCRIPTION)
      val polarity = yamlNodes.get(DomainOntology.POLARITY).get.asInstanceOf[Double]
      val filteredDescriptions =
          if (descriptions.isEmpty) descriptions
          else Some(descriptions.get.flatMap(filtered))

      ontologyNodes :+ new OntologyNode(route, name, polarity,  examples, filteredDescriptions)
    }

    protected def parseOntology(yamlNodes: Seq[Any], ontologyNodes: Seq[OntologyNode], crumbs: Seq[String]): Seq[OntologyNode] = {
      if (yamlNodes.nonEmpty) {
        val head = yamlNodes.head
        if (head.isInstanceOf[String])
          throw new Exception(s"Ontology has string (${head.asInstanceOf[String]}) where it should have a map.")
        val map: mutable.Map[String, JCollection[Any]] = head.asInstanceOf[JMap[String, JCollection[Any]]].asScala
        val key: String = map.keys.head
        val moreOntologyNodes =
            if (key == DomainOntology.FIELD) parseOntology(map, ontologyNodes, crumbs)
            else parseOntology(map(key).asScala.toSeq, ontologyNodes, key +: crumbs)

        parseOntology(yamlNodes.tail, moreOntologyNodes, crumbs)
      }
      else
        ontologyNodes
    }
  }

  def apply(name: String, ontologyPath: String, cachedDir: String, proc: Processor, filter: Boolean, loadFromSerialized: Boolean = false): DomainOntology = {
    if (loadFromSerialized)
      DomainOntology.load(serializedPath(name, cachedDir))
    else {
      logger.info("Processing yml ontology...")
      new DomainOntologyBuilder(name, ontologyPath, cachedDir, proc, filter).build()
    }
  }
}

// These are just here for when behavior might have to start differing.
object UNOntology {
  def apply(name: String, ontologyPath: String, cachedDir: String, proc: Processor, filter: Boolean = true, loadSerialized: Boolean = false) = DomainOntology(name, ontologyPath, cachedDir, proc, filter, loadSerialized)
}

object WDIOntology {
  def apply(name: String, ontologyPath: String, cachedDir: String, proc: Processor, filter: Boolean = true, loadSerialized: Boolean = false) = DomainOntology(name, ontologyPath, cachedDir, proc, filter, loadSerialized)
}

object FAOOntology {
  def apply(name: String, ontologyPath: String, cachedDir: String, proc: Processor, filter: Boolean = true, loadSerialized: Boolean = false) = DomainOntology(name, ontologyPath, cachedDir, proc, filter, loadSerialized)
}

object TopoFlowOntology {
  def apply(name: String, ontologyPath: String, cachedDir: String, proc: Processor, filter: Boolean = true, loadSerialized: Boolean = false) = DomainOntology(name, ontologyPath, cachedDir, proc, filter, loadSerialized)
}
