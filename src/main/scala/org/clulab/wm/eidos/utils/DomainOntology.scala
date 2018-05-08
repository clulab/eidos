package org.clulab.wm.eidos.utils

import java.util
import java.util.{ArrayList => JArrayList, Collection, Map => JMap}

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.wm.eidos.groundings.EidosWordToVec
import org.clulab.wm.eidos.utils.DomainOntology.{nlpProc, parseOntology}
import org.clulab.wm.eidos.utils.FileUtils.getTextFromResource
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable

class DomainOntology(concepts: Map[String, Seq[String]], filterOnPos: Boolean = false){

  def iterateOntology(wordToVec: EidosWordToVec): Map[String, Seq[Double]] = {
    for ((concept, examples) <- concepts) yield {
      val avgEmbedding = wordToVec.makeCompositeVector(examples.flatMap(_.split(" +")))
      (concept, avgEmbedding.toSeq)
    }
  }
}

object DomainOntology {
  val ROOT = ""

  lazy val nlpProc = new FastNLPProcessor

  def apply(forest: Collection[Any], filterOnPos: Boolean): DomainOntology = new DomainOntology(parseOntology(forest, filterOnPos))

  def parseOntology (nodes: Collection[Any], filterOnPos: Boolean = false): Map[String, Seq[String]] = {
    val concepts = mutable.Map.empty[String, Seq[String]]
    parseOntology(nodes.asScala.toSeq, "", Seq(), concepts, filterOnPos)
    // Following lines are for testing the creation of the ontology nodes. Can be removed later
//    val x = concepts.toMap
//    println("--------------------")
//    for (k <- x.keys){
//      println(s"${k} --> ${x.get(k).get.mkString(", ")}")
//    }
//    println("--------------------")
    concepts.toMap
  }

  // Note: modifying the odin.Taxonomy's mkParents() method
  def parseOntology (nodes: Seq[Any], path: String, terms: Seq[Any], preTerminals: mutable.Map[String, Seq[String]], filterOnPos: Boolean): Unit = nodes match {
    case Nil =>
      if (path.isEmpty || terms.length == 0) {
        return
      }
//    println(s"${path.asInstanceOf[String]} -> ${terms.asInstanceOf[Seq[String]]}")
      preTerminals.put(path.asInstanceOf[String], terms.asInstanceOf[Seq[String]])

    case (term: String) +: tail =>
//      println(s"Sentence: ${term}")
      if (filterOnPos){
        val posString = nlpProc.annotate(term)
        val filteredTerms = posString.sentences.flatMap{sent =>
             sent.words.zip(sent.tags.get).filter{w => w._2.contains("NN") || //filter by POS tags which need to be kept (Nouns, Adjectives and Verbs)
                                                       w._2.contains("JJ") ||
                                                       w._2.contains("VB")}
                                          .map(_._1) // get only the words
        }
//        println(s"Filtered Terms: ${filteredTerms.mkString(", ")}")
        parseOntology(tail, path, terms ++ filteredTerms, preTerminals, filterOnPos)
      }
      else{
        parseOntology(tail, path, terms ++ Seq(term), preTerminals, filterOnPos)
      }


    case head +: tail =>
      val map = head.asInstanceOf[JMap[String, Collection[Any]]].asScala
      if (map.keys.size != 1) {
        val labels = map.keys.mkString(", ")
        throw new Exception(s"taxonomy tree node with multiple labels: $labels")
      }
      val term = map.keys.head
      Option(map(term)) match {
        case None =>
          val msg = s"taxonomy term '$term' has no children (looks like an extra ':')"
          throw new Exception(msg)
        case Some(children) =>
          parseOntology(children.asScala.toSeq, path+"/"+term, terms, preTerminals, filterOnPos)
      }
      parseOntology(tail, path, terms, preTerminals, filterOnPos)
  }
}

class ToyOntology(ontologyPath: String, concepts: Map[String, Seq[String]], filterOnPos: Boolean = false) extends DomainOntology(concepts, filterOnPos) {

// Turn into key, values

  def apply(forest: Collection[Any], filterOnPos: Boolean): DomainOntology = new DomainOntology(parseOntology(forest, filterOnPos))
  val input = getTextFromResource(ontologyPath)
  val yaml = new Yaml(new Constructor(classOf[Collection[Any]]))
  val collection = yaml.load(input).asInstanceOf[Collection[Any]]

  val result = parseOntology(collection, filterOnPos)

  result.toSeq.foreach(println)

  def parseOntology (nodes: Collection[Any], filterOnPos: Boolean = false): Map[String, Seq[String]] = {
    val concepts = mutable.Map.empty[String, Seq[String]]
    parseOntology(nodes.asScala.toSeq, "", Seq.empty, concepts, filterOnPos)
    concepts.toMap
  }

  final def parseOntology (nodes: Seq[Any], path: String, terms: Seq[String], preTerminals: mutable.Map[String, Seq[String]], filterOnPos: Boolean): Unit = {
    if (nodes.isEmpty) {
      // Get to end of the line.
      if (path.nonEmpty && terms.length != 0)
        preTerminals.put(path, terms)
    }
    else {
      val head = nodes.head

      if (head.isInstanceOf[String])
        // Process a child.
        parseOntology(nodes.tail, path, terms :+ head.asInstanceOf[String], preTerminals, filterOnPos)
      else {
        val map = head.asInstanceOf[JMap[String, Collection[Any]]].asScala
        if (map.keys.size != 1)
          throw new Exception("taxonomy tree node with multiple labels: " + map.keys.mkString(", "))
        if (map.values.isEmpty)
            throw new Exception(s"taxonomy term '$map.keys.head' has no children (looks like an extra ':')")
        // Process the children.
        val key: String = map.keys.head
        parseOntology(map(key).asScala.toSeq, path + "/" + key, terms, preTerminals, filterOnPos)
        // Continue with siblings.
        parseOntology(nodes.tail, path, terms, preTerminals, filterOnPos)
      }
    }
  }
}

class UNOntology(ontologyPath: String, concepts: Map[String, Seq[String]], filterOnPos: Boolean = false) extends DomainOntology(concepts, filterOnPos) {
// turn into key, examples

  def apply(forest: Collection[Any], filterOnPos: Boolean): DomainOntology = new DomainOntology(parseOntology(forest, filterOnPos))
  val input = getTextFromResource(ontologyPath)
  val yaml = new Yaml(new Constructor(classOf[Collection[Any]]))
  val collection = yaml.load(input).asInstanceOf[Collection[Any]]

  val result = parseOntology(collection, filterOnPos)

  result.toSeq.foreach(println)

  def parseOntology (nodes: Collection[Any], filterOnPos: Boolean = false): Map[String, Seq[String]] = {
    val concepts = mutable.Map.empty[String, Seq[String]]
    parseOntology(nodes.asScala.toSeq, "", Seq.empty, concepts, filterOnPos)
    concepts.toMap
  }

  final def parseOntology (nodes: Seq[Any], path: String, terms: Seq[String], preTerminals: mutable.Map[String, Seq[String]], filterOnPos: Boolean): Unit = {
    if (nodes.isEmpty) {
      // Get to end of the line.
      if (path.nonEmpty && terms.length != 0) {
//        if (!path.endsWith("/examples"))
//          throw new Exception("Path found that does not end with /examples")
        preTerminals.put(path, terms) // may need to edit path to remove examples
      }
    }
    else {
      val head = nodes.head

      if (head.isInstanceOf[String])
      // Process a child.
        parseOntology(nodes.tail, path, terms :+ head.asInstanceOf[String], preTerminals, filterOnPos)
      else {
        val map = head.asInstanceOf[JMap[String, Collection[Any]]].asScala
        if (map.keys.size != 1)
          throw new Exception("taxonomy tree node with multiple labels: " + map.keys.mkString(", "))
        if (map.values.isEmpty)
          throw new Exception(s"taxonomy term '$map.keys.head' has no children (looks like an extra ':')")
        // Process the children.
        val key: String = map.keys.head // Look for examples
        parseOntology(map(key).asScala.toSeq, path + "/" + key, terms, preTerminals, filterOnPos)
        // Continue with siblings.
        parseOntology(nodes.tail, path, terms, preTerminals, filterOnPos)
      }
    }
  }
}

class WDIOntology(ontologyPath: String, concepts: Map[String, Seq[String]], filterOnPos: Boolean = false) extends DomainOntology(concepts, filterOnPos) {
  def apply(forest: Collection[Any], filterOnPos: Boolean): DomainOntology = new DomainOntology(parseOntology(forest, filterOnPos))
  val input = getTextFromResource(ontologyPath)
  val yaml = new Yaml(new Constructor(classOf[Collection[Any]]))
  val collection = yaml.load(input).asInstanceOf[Collection[Any]]

  val result = parseOntology(collection, filterOnPos)

  result.toSeq.foreach(println)

  def parseOntology (nodes: Collection[Any], filterOnPos: Boolean = false): Map[String, Seq[String]] = {
    val concepts = mutable.Map.empty[String, Seq[String]]
    parseOntology(nodes.asScala.toSeq, "", Seq.empty, concepts, filterOnPos)
    concepts.toMap
  }

  final def parseOntology (nodes: Seq[Any], path: String, terms: Seq[String], preTerminals: mutable.Map[String, Seq[String]], filterOnPos: Boolean): Unit = {
    if (nodes.isEmpty) {
      // Get to end of the line.
      if (path.nonEmpty && terms.length != 0) {
        if (!path.endsWith("/description"))
          throw new Exception("Path found that does not end with /description")
        preTerminals.put(path, terms) // may need to edit path to remove examples
      }
    }
    else {
      val head = nodes.head

      if (head.isInstanceOf[String])
      // Process a child.
        parseOntology(nodes.tail, path, terms :+ head.asInstanceOf[String], preTerminals, filterOnPos)
      else {
        val map = head.asInstanceOf[JMap[String, Collection[Any]]].asScala
        if (map.keys.size != 1)
          throw new Exception("taxonomy tree node with multiple labels: " + map.keys.mkString(", "))
        if (map.values.isEmpty)
          throw new Exception(s"taxonomy term '$map.keys.head' has no children (looks like an extra ':')")
        // Process the children.
        val key: String = map.keys.head // Look for examples
        parseOntology(map(key).asScala.toSeq, path + "/" + key, terms, preTerminals, filterOnPos)
        // Continue with siblings.
        parseOntology(nodes.tail, path, terms, preTerminals, filterOnPos)
      }
    }
  }
}

class FAOOntology(ontologyPath: String, concepts: Map[String, Seq[String]], filterOnPos: Boolean = false) extends DomainOntology(concepts, filterOnPos) {

  def apply(forest: Collection[Any], filterOnPos: Boolean): DomainOntology = new DomainOntology(parseOntology(forest, filterOnPos))
  val input = getTextFromResource(ontologyPath)
  val yaml = new Yaml(new Constructor(classOf[Collection[Any]]))
  val collection = yaml.load(input).asInstanceOf[Collection[Any]]

  val result = parseOntology(collection, filterOnPos)

  result.toSeq.foreach(println)

  def parseOntology (nodes: Collection[Any], filterOnPos: Boolean = false): Map[String, Seq[String]] = {
    val concepts = mutable.Map.empty[String, Seq[String]]
    parseOntology(nodes.asScala.toSeq, "", Seq.empty, concepts, filterOnPos)
    concepts.toMap
  }

  final def parseOntology (nodes: Seq[Any], path: String, terms: Seq[String], preTerminals: mutable.Map[String, Seq[String]], filterOnPos: Boolean): Unit = {
    if (nodes.isEmpty) {
      // Get to end of the line.
      if (path.nonEmpty && terms.length != 0) {
                if (!path.endsWith("/description"))
                  throw new Exception("Path found that does not end with /examples")
        preTerminals.put(path, terms) // may need to edit path to remove examples
      }
    }
    else {
      val head = nodes.head

      if (head.isInstanceOf[String])
      // Process a child.
        parseOntology(nodes.tail, path, terms :+ head.asInstanceOf[String], preTerminals, filterOnPos)
      else {
        val map = head.asInstanceOf[JMap[String, Collection[Any]]].asScala
        if (map.keys.size != 1)
          throw new Exception("taxonomy tree node with multiple labels: " + map.keys.mkString(", "))
        if (map.values.isEmpty)
          throw new Exception(s"taxonomy term '$map.keys.head' has no children (looks like an extra ':')")
        // Process the children.
        val key: String = map.keys.head // Look for examples
        parseOntology(map(key).asScala.toSeq, path + "/" + key, terms, preTerminals, filterOnPos)
        // Continue with siblings.
        parseOntology(nodes.tail, path, terms, preTerminals, filterOnPos)
      }
    }
  }
}
