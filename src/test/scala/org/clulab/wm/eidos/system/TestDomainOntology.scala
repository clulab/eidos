package org.clulab.wm.eidos.system

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.test.TestUtils._

class TestDomainOntology extends Test {

  def hasDuplicates(domainOntology: DomainOntology): Boolean = {
    val ontologyNodes = domainOntology.ontologyNodes
    val pathSeq = ontologyNodes.map(_.route())
    val pathSet = pathSeq.toSet

    if (pathSeq.size != pathSet.size) {
      val pathBag = pathSeq.foldLeft(Map[String, Int]())((map, path) => map + (path -> (map.getOrElse(path, 0) + 1)))
      val duplicates = pathBag.toSeq.filter(_._2 > 1).map(_._1).mkString("\n\t")

      println(s"""The domain ontology "${domainOntology.name}" includes duplicate paths:\n\t${duplicates}""")
      true
    }
    else
      false
  }

  val proc = new FastNLPProcessor()

  // Do test classes have access to main resources?  Yes, apparently.
  // These paths must be coordinated with default values in EidosSystem.

  behavior of "toy ontology"
  it should "load and not have duplicates" in {
    hasDuplicates(ToyOntology("toy", "/org/clulab/wm/eidos/ontologies/toy_ontology.yml", proc)) should be (false)
  }

  behavior of "un ontology"
  it should "load and not have duplicates" in {
    hasDuplicates(UNOntology("un", "/org/clulab/wm/eidos/ontologies/un_ontology.yml", proc)) should be (false)
  }

  behavior of "fao ontology"
  it should "load and not have duplicates" in {
    hasDuplicates(FAOOntology("fao", "/org/clulab/wm/eidos/ontologies/fao_variable_ontology.yml", proc)) should be (false)
  }

  behavior of "wdi ontology"
  it should "load" in {
    hasDuplicates(WDIOntology("wdi", "/org/clulab/wm/eidos/ontologies/wdi_ontology.yml", proc)) should be (false)
  }
}
