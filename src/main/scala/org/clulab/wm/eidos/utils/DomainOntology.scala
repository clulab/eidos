package org.clulab.wm.eidos.utils

import java.util.{Collection, Map => JMap}

import org.clulab.embeddings.word2vec.Word2Vec

import scala.collection.JavaConverters._
import scala.collection.mutable

class DomainOntology(concepts: Map[String, Seq[String]]){

  def iterateOntology(w2v: Word2Vec): Map[String, Seq[Double]] = {
    for ((concept, examples) <- concepts) yield {
      val avgEmbedding = w2v.makeCompositeVector(examples.flatMap(_.split(" +")))
      (concept, avgEmbedding.toSeq)
    }
  }

}

object DomainOntology {
  val ROOT = ""

  def apply(forest: Collection[Any]): DomainOntology = new DomainOntology(parseOntology(forest))

  def parseOntology (nodes: Collection[Any]): Map[String, Seq[String]] = {
    val concepts = mutable.Map.empty[String, Seq[String]]
    parseOntology(nodes.asScala.toSeq, "", Seq(), concepts)
    concepts.toMap
  }

  // Note: modifying the odin.Taxonomy's mkParents() method
  def parseOntology (nodes: Seq[Any], path: String, terms: Seq[Any], preTerminals: mutable.Map[String, Seq[String]]): Unit = nodes match {
    case Nil =>
      if (path.isEmpty || terms.length == 0) {
        return
      }
//    println(s"${path.asInstanceOf[String]} -> ${terms.asInstanceOf[Seq[String]]}")
      preTerminals.put(path.asInstanceOf[String], terms.asInstanceOf[Seq[String]])

    case (term: String) +: tail =>
      parseOntology(tail, path, terms ++ Seq(term), preTerminals)
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
          parseOntology(children.asScala.toSeq, path+"/"+term, terms, preTerminals)
      }
      parseOntology(tail, path, terms, preTerminals)
  }

}