package org.clulab.wm.eidos.groundings

import java.util.{Collection => JCollection, Map => JMap}

import org.clulab.processors.Processor
import org.clulab.wm.eidos.utils.FileUtils.getTextFromResource
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.collection.JavaConverters._
import scala.collection.mutable

class OntologyNode(val path: Seq[String], val name: String, examples: Option[Seq[String]] = None, description: Option[Seq[String]] = None) {

  protected def split(values: Option[Seq[String]]): Seq[String] =
      if (values.isEmpty) Seq.empty
      else values.get.flatMap(_.split(" +"))

  // Can there already be a / in any of the paths that must be escaped?
  def route(): String =
      path.mkString(OntologyNode.SEPARATOR) + (if (path.isEmpty) "" else OntologyNode.SEPARATOR) + name

  // Right now it doesn't matter where these come from (as long as descriptions are filtered).
  val values: Seq[String] = split(examples) ++ description.getOrElse(Seq.empty)

  override def toString = route() + " = " + values.toList
}

object OntologyNode {
  val SEPARATOR = "/"
}

class DomainOntology(val name: String, val ontologyNodes: Seq[OntologyNode]) {

  def iterateOntology(wordToVec: EidosWordToVec): Seq[ConceptEmbedding] =
      ontologyNodes.map { ontologyNode =>
        ConceptEmbedding(ontologyNode.route(), wordToVec.makeCompositeVector(ontologyNode.values))
      }
}

object DomainOntology {
  val FIELD = "OntologyNode"
  val NAME = "name"
  val EXAMPLES = "examples"
  val DESCRIPTION = "description"

  // This is mostly here to capture proc so that it doesn't have to be passed around.
  class DomainOntologyBuilder(name: String, ontologyPath: String, proc: Processor) {

    def build(): DomainOntology = {
      val text = getTextFromResource(ontologyPath)
      val yaml = new Yaml(new Constructor(classOf[JCollection[Any]]))
      val yamlNodes = yaml.load(text).asInstanceOf[JCollection[Any]].asScala.toSeq
      val ontologyNodes = parseOntology(yamlNodes, Seq.empty, Seq.empty)

      new DomainOntology(name, ontologyNodes)
    }

    def filtered(text: String): Seq[String] = {
      text.split(" +")

      // TODO: Return this functionality after finish testing
//      val posString = proc.annotate(text)
//      val filteredTerms = posString.sentences.flatMap { sentence =>
//        sentence.words.zip(sentence.tags.get).filter { wordAndPos =>
//          wordAndPos._2.contains("NN") || // Filter by POS tags which need to be kept (Nouns, Adjectives and Verbs).
//              wordAndPos._2.contains("JJ") ||
//              wordAndPos._2.contains("VB")
//        }.map(_._1) // Get only the words.
//      }
////      println(s"Filtered Terms: ${filteredTerms.mkString(", ")}")
//      filteredTerms
    }

    def parseOntology(yamlNodes: mutable.Map[String, JCollection[Any]], ontologyNodes: Seq[OntologyNode], path: Seq[String]): Seq[OntologyNode] = {
      val name: String = yamlNodes.get(DomainOntology.NAME).get.asInstanceOf[String]
      val examples: Option[Seq[String]] = yamlNodes.get(DomainOntology.EXAMPLES).map(_.asInstanceOf[JCollection[String]].asScala.toSeq)
      val yamlDescription: Option[JCollection[Any]] = yamlNodes.get(DomainOntology.DESCRIPTION)
      val description: Option[String] =
          if (yamlDescription.isDefined) Some(yamlDescription.get.asInstanceOf[String])
          else None

      ontologyNodes :+ new OntologyNode(path, name, examples, description.map(filtered))
    }

    def parseOntology(yamlNodes: Seq[Any], ontologyNodes: Seq[OntologyNode], path: Seq[String]): Seq[OntologyNode] = {
      if (yamlNodes.nonEmpty) {
        val map: mutable.Map[String, JCollection[Any]] = yamlNodes.head.asInstanceOf[JMap[String, JCollection[Any]]].asScala
        val key: String = map.keys.head
        val moreOntologyNodes =
            if (key == DomainOntology.FIELD) parseOntology(map, ontologyNodes, path)
            else parseOntology(map(key).asScala.toSeq, ontologyNodes, path :+ key)

        parseOntology(yamlNodes.tail, moreOntologyNodes, path)
      }
      else
        ontologyNodes
    }
  }

  def apply(name: String, ontologyPath: String, proc: Processor): DomainOntology =
      new DomainOntologyBuilder(name, ontologyPath, proc).build()
}

// These are just here for when behavior might have to start differing.
object ToyOntology {
  def apply(name: String, ontologyPath: String, proc: Processor) = DomainOntology(name, ontologyPath, proc)
}

object UNOntology {
  def apply(name: String, ontologyPath: String, proc: Processor) = DomainOntology(name, ontologyPath, proc)
}

object WDIOntology {
  def apply(name: String, ontologyPath: String, proc: Processor) = DomainOntology(name, ontologyPath, proc)
}

object FAOOntology {
  def apply(name: String, ontologyPath: String, proc: Processor) = DomainOntology(name, ontologyPath, proc)
}
