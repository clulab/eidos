package org.clulab.wm.eidos.system

import java.util.IdentityHashMap

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.utils.Timer

import scala.collection.JavaConverters._

import scala.collection.mutable

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
  val filter = false

  // These paths must be coordinated with default values in EidosSystem.

  behavior of "un ontology"
  it should "load and not have duplicates" in {
    Timer.time("Load UN without cache") {
      UNOntology("un", "/org/clulab/wm/eidos/ontologies/un_ontology.yml", "./cache/", proc, filter, loadSerialized = false)
    }
    Timer.time("Load UN with cache") {
      UNOntology("un", "/org/clulab/wm/eidos/ontologies/un_ontology.yml", "./cache/", proc, filter, loadSerialized = true)
    }
  }

  behavior of "fao ontology"
  it should "load and not have duplicates" in {
    Timer.time("Load FAO without cache") {
      FAOOntology("fao", "/org/clulab/wm/eidos/ontologies/fao_variable_ontology.yml", "./cache/", proc, filter, loadSerialized =false)
    }
    Timer.time("Load FAO with cache") {
      FAOOntology("fao", "/org/clulab/wm/eidos/ontologies/fao_variable_ontology.yml", "./cache/", proc, filter, loadSerialized = true)
    }
  }

  behavior of "wdi ontology"
  it should "load and not have duplicates" in {
    Timer.time("Load WDI without cache") {
      WDIOntology("wdi", "/org/clulab/wm/eidos/ontologies/wdi_ontology.yml", "./cache/", proc, filter, loadSerialized = false)
    }
    Timer.time("Load WDI with cache") {
      WDIOntology("wdi", "/org/clulab/wm/eidos/ontologies/wdi_ontology.yml", "./cache/", proc, filter, loadSerialized = true)
    }
  }

  behavior of "mesh ontology"
  it should "load and not have duplicates" in {
    Timer.time("Load MeSH without cache") {
      MeshOntology("mesh", "/org/clulab/wm/eidos/ontologies/mesh_ontology.yml", "./cache/", proc, filter, loadSerialized = false)
    }
    val mesh = Timer.time("Load MeSH with cache") {
      MeshOntology("mesh", "/org/clulab/wm/eidos/ontologies/mesh_ontology.yml", "./cache/", proc, filter, loadSerialized = true)
    }

//    mesh.ontologyNodes.foreach { ontologyNode =>
//      println(ontologyNode.name)
//    }

    val mapOfNodes = new IdentityHashMap[OntologyNode, String]()
    mesh.ontologyNodes.foreach { ontologyNode =>
      mapOfNodes.put(ontologyNode, ontologyNode.nodeName)

      var branchNode = ontologyNode.parent
      while (branchNode != null) {
        mapOfNodes.put(branchNode, branchNode.nodeName)
        branchNode = branchNode.parent
      }
    }

//    mapOfNodes.keySet.asScala.foreach { ontologyNode =>
//      println(ontologyNode.nodeName)
//    }

    hasDuplicates(mesh) should be (false)
  }
}
