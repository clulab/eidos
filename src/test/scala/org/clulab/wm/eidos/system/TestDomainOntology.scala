package org.clulab.wm.eidos.system

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.utils.Timer

class TestDomainOntology extends Test {

  def hasDuplicates(domainOntology: DomainOntology): Boolean = {
    val pathSeq = 0.until(domainOntology.size).map { i => domainOntology.getNamer(i).name }
    val pathSet = pathSeq.toSet

//    println(s"""The domain ontology "${domainOntology.name}" node count: ${ontologyNodes.length}""")
//    ontologyNodes.foreach(println)

    if (pathSeq.size != pathSet.size) {
      val pathBag = pathSeq.foldLeft(Map[String, Int]())((map, path) => map + (path -> (map.getOrElse(path, 0) + 1)))
      val duplicatePaths = pathBag.toSeq.filter(_._2 > 1).map(_._1)

      println(s"""The domain ontology "${domainOntology.name}" includes duplicate nodes:""")
      duplicatePaths.foreach(println)
      true
    }
    else
      false
  }

  val proc = new FastNLPProcessor()
  val filter = true

  // These paths must be coordinated with default values in EidosSystem.

  behavior of "un ontology"
  it should "load and not have duplicates" in {
    hasDuplicates(UNOntology("un", "/org/clulab/wm/eidos/ontologies/un_ontology.yml", "", proc, filter)) should be (false)
  }

  behavior of "fao ontology"
  it should "load and not have duplicates" in {
    hasDuplicates(FAOOntology("fao", "/org/clulab/wm/eidos/ontologies/fao_variable_ontology.yml", "", proc, filter)) should be (false)
  }

  behavior of "wdi ontology"
  it should "load and not have duplicates" in {
    hasDuplicates(WDIOntology("wdi", "/org/clulab/wm/eidos/ontologies/wdi_ontology.yml", "", proc, filter)) should be (false)
  }

  // TODO: This one appears to have many duplicates.
  behavior of "topoFlow ontology"
  ignore should "load and not have duplicates" in {
    hasDuplicates(TopoFlowOntology("topo", "/org/clulab/wm/eidos/ontologies/topoflow_ontology.yml", "", proc, filter)) should be (false)
  }

  behavior of "mesh ontology"
  it should "load and not have duplicates" in {
    val mesh = Timer.time("Loading mesh") {
      MeshOntology("mesh", "/org/clulab/wm/eidos/ontologies/mesh_ontology.yml", "", proc, filter)
    }

    hasDuplicates(mesh) should be (false)
  }
}
