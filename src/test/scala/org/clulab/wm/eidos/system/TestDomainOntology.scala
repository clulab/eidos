package org.clulab.wm.eidos.system

import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.CompactDomainOntology.CompactDomainOntologyBuilder
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.utils.{Canonicalizer, Timer}

class TestDomainOntology extends Test {

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
  val proc = reader.proc
  val canonicalizer = new Canonicalizer(reader.stopwordManager)
  val convert = true
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

  def cachePath(name: String): String = s"./cache/english/${name}.serialized"

  behavior of "un ontology"
  it should "load and not have duplicates" in {
    val path = baseDir + "/un_ontology.yml"

    val newOntology = Timer.time("Load UN without cache") {
      DomainOntologies(path, "", proc, canonicalizer, filter, useCache = false)
    }
    val newerOntology =
      if (convert)
        Timer.time("Convert UN to compact") {
          new CompactDomainOntologyBuilder(newOntology.asInstanceOf[TreeDomainOntology]).build
        }
      else
        Timer.time("Load UN from cache") {
          DomainOntologies(path, "", proc, canonicalizer, filter, useCache = true)
        }

//    val newestOntology = Timer.time("Load UN from cache") {
//      UNOntology("", cachePath("un"), proc, canonicalizer, filter, useCache = true)
//    }
//
//    show3(newOntology, newerOntology, newestOntology)

    hasDuplicates("un", newOntology) should be (false)
    hasDuplicates("un", newerOntology) should be (false)
  }

  behavior of "mitre12 ontology"
  it should "load and not have duplicates" in {
    val path = baseDir + "/mitre12_indicators.yml"

    val newOntology = Timer.time("Load MITRE12 without cache") {
      DomainOntologies(path, "", proc, canonicalizer, filter, useCache =false)
    }
    val newerOntology =
      if (convert)
        Timer.time("Convert MITRE12 to compact") {
          new CompactDomainOntologyBuilder(newOntology.asInstanceOf[TreeDomainOntology]).build
        }
      else
        Timer.time("Load MITRE12 from cache") {
          DomainOntologies(path, "", proc, canonicalizer, filter, useCache = true)
        }

//    val newestOntology = Timer.time("Load MITRE12 with cache") {
//      FAOOntology("", cachePath("mitre12"), proc, canonicalizer, filter, useCache = true)
//    }
//
//    show3(newOntology, newerOntology, newestOntology)

    hasDuplicates("mitre12", newOntology) should be (false)
    hasDuplicates("mitre12", newerOntology) should be (false)
  }

  // TODO: This one appears to have many duplicates.
  behavior of "topoFlow ontology"
  ignore should "load and not have duplicates" in {
    val path = baseDir + "/topoflow_ontology.yml"

    val newOntology = Timer.time("Load TOPO without cache") {
      DomainOntologies(path, "", proc, canonicalizer, filter, useCache = false)
    }

    hasDuplicates("topo", newOntology) should be (false)
  }

  behavior of "mesh ontology"
  ignore should "load and not have duplicates" in {
    val path = baseDir + "/mesh_ontology.yml"

    val newOntology = Timer.time("Load MeSH without cache") {
      DomainOntologies(path, "", proc, canonicalizer, filter, useCache = false)
    }
    val newerOntology =
      if (convert)
        Timer.time("Convert MeSH to compact") {
          new CompactDomainOntologyBuilder(newOntology.asInstanceOf[TreeDomainOntology]).build
        }
      else
        Timer.time("Load MeSH from cache") {
          DomainOntologies(path, "", proc, canonicalizer, filter, useCache = true)
        }

//    val newestOntology = Timer.time("Load MeSH with cache") {
//      MeshOntology("", cachePath("mesh"), proc, canonicalizer, filter, useCache = true)
//    }
//
//    show3(newOntology, newerOntology, newestOntology)

    hasDuplicates("mesh", newOntology) should be (false)
    hasDuplicates("mesh", newerOntology) should be (false)
  }

  behavior of "props ontology"
  it should "load and not have duplicates" in {
    val path = baseDir + "/un_properties.yml"

    val newOntology = Timer.time("Load UN properties without cache") {
      DomainOntologies(path, "", proc, canonicalizer, filter, useCache = false)
    }
    val newerOntology =
      if (convert)
        Timer.time("Convert UN properties to compact") {
          new CompactDomainOntologyBuilder(newOntology.asInstanceOf[TreeDomainOntology]).build
        }
      else
        Timer.time("Load UN properties from cache") {
          DomainOntologies(path, "", proc, canonicalizer, filter, useCache = true)
        }

//    val newestOntology = Timer.time("Load UN properties from cache") {
//      PropertiesOntology("", cachePath("props"), proc, canonicalizer, filter, useCache = true)
//    }
//
//    show3(newOntology, newerOntology, newestOntology)

    hasDuplicates("props", newOntology) should be (false)
    hasDuplicates("props", newerOntology) should be (false)
  }

  behavior of "wm ontology"
  it should "load and not have duplicates" in {
    val path = baseDir + "/wm_metadata.yml"

    val newOntology = Timer.time("Load WM without cache") {
      DomainOntologies(path, "", proc, canonicalizer, filter, useCache = false)
    }
    val newerOntology =
      if (convert)
        Timer.time("Convert WM to compact") {
          new CompactDomainOntologyBuilder(newOntology.asInstanceOf[TreeDomainOntology]).build
        }
      else
        Timer.time("Load WM from cache") {
          DomainOntologies(path, "", proc, canonicalizer, filter, useCache = true)
        }

    //    val newestOntology = Timer.time("Load UN from cache") {
    //      UNOntology("", cachePath("un"), proc, canonicalizer, filter, useCache = true)
    //    }
    //
    //    show3(newOntology, newerOntology, newestOntology)

    hasDuplicates("wm", newOntology) should be (false)
    hasDuplicates("wm", newerOntology) should be (false)
  }
}
