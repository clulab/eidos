package org.clulab.wm.eidos.utils

import java.util.{Collection, Map => JMap}
import scala.collection.JavaConverters._

import org.clulab.odin.impl.{OdinCompileException, Taxonomy}

class DomainOntology(parents: Map[String, String]) extends Taxonomy(parents) {

  def iterateOntology(): Unit ={

  }


}

object DomainOntology {
  val ROOT = "**ROOT**"

  def apply(forest: Collection[Any]): DomainOntology = new DomainOntology(mkParents(forest))

  def mkParents(nodes: Collection[Any]): Map[String, String] =
    mkParents(nodes.asScala.toSeq, ROOT, Map.empty)

  def parse (nodes: Seq[Any], parent: String): Unit = nodes match {
    case Nil => println("DONE PARSING")
    case (term: String) +: tail =>
      println(s"PARSING $term")
      parse(tail, parent)
    case head +: tail =>
      println (s"MOVING ON ...HEAD=${head.toString} .. TAIL=${tail.toString}")
      parse(tail, parent)

  }

  def mkParents(
                 nodes: Seq[Any],
                 parent: String,
                 table: Map[String,String]
               ): Map[String, String] = nodes match {
    case Nil =>
      // we are done parsing, return the parents table
      table
    case (term: String) +: tail =>
      if (table contains term) {
        throw new OdinCompileException(s"duplicated taxonomy term '$term'")
      }
      // add term to parents table and continue parsing siblings
      mkParents(tail, parent, table.updated(term, parent))
    case head +: tail =>
      // get next node as a scala map
      val map = head.asInstanceOf[JMap[String, Collection[Any]]].asScala
      if (map.keys.size != 1) {
        val labels = map.keys.mkString(", ")
        throw new OdinCompileException(s"taxonomy tree node with multiple labels: $labels")
      }
      val term = map.keys.head
      if (table contains term) {
        throw new OdinCompileException(s"duplicated taxonomy term '$term'")
      }
      Option(map(term)) match {
        case None =>
          val msg = s"taxonomy term '$term' has no children (looks like an extra ':')"
          throw new OdinCompileException(msg)
        case Some(children) =>
          // 1. add term to parents table
          // 2. parse children
          // 3. parse siblings
          mkParents(tail, parent, mkParents(children.asScala.toSeq, term, table.updated(term, parent)))
      }
  }
}