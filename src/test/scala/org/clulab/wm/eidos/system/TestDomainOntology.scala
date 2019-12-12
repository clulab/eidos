package org.clulab.wm.eidos.system

import com.typesafe.config.{ConfigFactory, ConfigValueFactory}

import ai.lum.common.ConfigUtils._
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.CompactDomainOntology.CompactDomainOntologyBuilder
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.utils.{Canonicalizer, Timer}

class TestDomainOntology extends Test {

  def matches(left: DomainOntology, right: DomainOntology): Boolean = {

    def getNames(domainOntology: DomainOntology): Seq[String] = {
      0.until(domainOntology.size).map { index =>
        domainOntology.getNamer(index).name
      }
    }

    def getValues(domainOntology: DomainOntology): Seq[Array[String]] = {
      0.until(domainOntology.size).map { index =>
        domainOntology.getValues(index)
      }
    }

    val leftNames = getNames(left)
    val rightNames = getNames(right)

    val leftValues = getValues(left)
    val rightValues = getValues(right)

    leftNames == rightNames && leftValues == rightValues
  }

  def hasDuplicates(name: String, domainOntology: DomainOntology): Boolean = {
    val pathSeq = 0.until(domainOntology.size).map { i => domainOntology.getNamer(i).name }
    val pathSet = pathSeq.toSet

//    println(s"""The domain ontology "${domainOntology.name}" node count: ${ontologyNodes.length}""")
//    ontologyNodes.foreach(println)

    if (pathSeq.size != pathSet.size) {
      val pathBag = pathSeq.foldLeft(Map[String, Int]())((map, path) => map + (path -> (map.getOrElse(path, 0) + 1)))
      val duplicatePaths = pathBag.toSeq.filter(_._2 > 1).map(_._1)

      println(s"""The domain ontology "$name" includes duplicate nodes:""")
      duplicatePaths.foreach(println)
      true
    }
    else
      false
  }

  val baseDir = "/org/clulab/wm/eidos/english/ontologies"
  val config = ConfigFactory.load("eidos")
      .withValue("EidosSystem.useW2V", ConfigValueFactory.fromAnyRef(false, "Don't use vectors when caching ontologies."))
  val reader = new EidosSystem(config)
  val proc = reader.components.proc
  val canonicalizer = new Canonicalizer(reader.components.stopwordManager)
  val useCache = config[Boolean]("ontologies.useCache")
  val filter = true

  def show1(ontology: DomainOntology): Unit = {
    0.until(ontology.size).foreach { i =>
      println(ontology.getNamer(i).name + " = " + ontology.getValues(i).mkString(", "))
      ontology.getPatterns(i).map(_.foreach(regex => println(regex.toString)))
    }
    println
  }

  def show3(newOntology: DomainOntology, newerOntology: DomainOntology, newestOntology: DomainOntology): Unit = {
    show1(newOntology)
    show1(newerOntology)
    show1(newestOntology)
  }

  def testOntology(abbrev: String, name: String, path: String): Unit = {

    def cachePath(name: String): String = s"./cache/english/${name}.serialized"

    behavior of name

    it should "load and not have duplicates" in {
      val newOntology = Timer.time(s"Load $name without cache") {
        DomainOntologies(baseDir + path, "", proc, canonicalizer, filter, useCache = false)
      }
      hasDuplicates(name, newOntology) should be (false)

      val newerOntology = Timer.time(s"Convert $name to compact") {
        new CompactDomainOntologyBuilder(newOntology.asInstanceOf[TreeDomainOntology]).build
      }
      hasDuplicates(name, newerOntology) should be (false)
      matches(newOntology, newerOntology)

//      if (useCache) {
//        val newestOntology = Timer.time(s"Load $name from cache") {
//          DomainOntologies("", cachePath(abbrev), proc, canonicalizer, filter, useCache = true)
//        }
//
//        show3(newOntology, newerOntology, newestOntology)
//        hasDuplicates(name, newestOntology) should be (false)
//        matches(newOntology, newestOntology)
//      }
    }
  }

  testOntology("un", "un ontology", "/un_ontology.yml")
  testOntology("mitre12", "mitre12 ontology", "/mitre12_indicators.yml")
//  testOntology("topo", "topoFlow ontology", "/topoflow_ontology.yml")
//  testOntology("mesh", "mesh ontology", "/mesh_ontology.yml")
  testOntology("props", "props ontology", "/un_properties.yml")
  testOntology("wm", "wm ontology", "/wm_metadata.yml")
  testOntology("wm_flattened", "wm flattened ontology", "/wm_with_flattened_interventions_metadata.yml")
}
