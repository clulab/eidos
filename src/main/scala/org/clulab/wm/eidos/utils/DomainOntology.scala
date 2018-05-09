package org.clulab.wm.eidos.utils

import java.util.{Collection => JCollection, Map => JMap}

import org.clulab.processors.Processor
import org.clulab.wm.eidos.groundings.EidosWordToVec
import org.clulab.wm.eidos.utils.FileUtils.getTextFromResource
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.collection.JavaConverters._

class OntologyNode(val path: String, others: Seq[String], examples: Seq[String],  descriptions: Seq[String]) {
  protected def split(values: Seq[String]): Seq[String] = values.flatMap(_.split(" +"))

  // Right now it doesn't matter where these come from (as long as descriptions are filtered).
  val values: Seq[String] = split(others) ++ split(examples) ++ split(descriptions)
}

class DomainOntology(val name: String, protected val ontologyNodes: Seq[OntologyNode]) {

  def iterateOntology(wordToVec: EidosWordToVec): Seq[(String, Array[Double])] = {
    ontologyNodes.map { ontologyNode =>
      val avgEmbedding = wordToVec.makeCompositeVector(ontologyNode.values)
      (ontologyNode.path, avgEmbedding)
    }
  }
}

object DomainOntology {

  // This is mostly here to capture proc so that it doesn't have to be passed around.
  class DomainOntologyBuilder(name: String, ontologyPath: String, proc: Processor) {

    def build(): DomainOntology = {
      val text = getTextFromResource(ontologyPath)
      val yaml = new Yaml(new Constructor(classOf[JCollection[Any]]))
      val yamlNodes = yaml.load(text).asInstanceOf[JCollection[Any]].asScala.toSeq
      val ontologyNodes = parseOntology(yamlNodes, "", Seq.empty, Seq.empty, Seq.empty, Seq.empty)

      new DomainOntology(name, ontologyNodes)
    }

    def filtered(text: String): Seq[String] = {
      val posString = proc.annotate(text)
      val filteredTerms = posString.sentences.flatMap { sentence =>
        sentence.words.zip(sentence.tags.get).filter { wordAndPos =>
          wordAndPos._2.contains("NN") || // Filter by POS tags which need to be kept (Nouns, Adjectives and Verbs).
              wordAndPos._2.contains("JJ") ||
              wordAndPos._2.contains("VB")
        }.map(_._1) // Get only the words.
      }
//      println(s"Filtered Terms: ${filteredTerms.mkString(", ")}")
      filteredTerms
    }

    def parseOntology(yamlNodes: Seq[Any], path: String, ontologyNodes: Seq[OntologyNode],
        others: Seq[String], examples: Seq[String], descriptions: Seq[String]): Seq[OntologyNode] = {
      if (yamlNodes.isEmpty)
        // Get to end of the line.
        if (path.nonEmpty && (others.nonEmpty || examples.nonEmpty || descriptions.nonEmpty))
          ontologyNodes :+ new OntologyNode(path, others, examples, descriptions)
        else
          ontologyNodes
      else {
        val head = yamlNodes.head

        if (head.isInstanceOf[String]) {
          // Process a child.
          val value: String = head.asInstanceOf[String]
          if (path.endsWith("/example"))
            parseOntology(yamlNodes.tail, path, ontologyNodes, others, examples :+ value, descriptions)
          else if (path.endsWith("/description"))
            parseOntology(yamlNodes.tail, path, ontologyNodes, others, examples, descriptions ++ filtered(value))
          else
            parseOntology(yamlNodes.tail, path, ontologyNodes, others :+ value, examples, descriptions)
        }
        else {
          val map = head.asInstanceOf[JMap[String, JCollection[Any]]].asScala
          if (map.values.isEmpty)
            throw new Exception(s"taxonomy term '$map.keys.head' has no children (looks like an extra ':')")
          // Process the children.
          val key: String = map.keys.head
          val moreOntologyNodes = parseOntology(map(key).asScala.toSeq, path + "/" + key, ontologyNodes, others, examples, descriptions)
          // Continue with siblings.
          parseOntology(yamlNodes.tail, path, moreOntologyNodes, others, examples, descriptions)
        }
      }
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
