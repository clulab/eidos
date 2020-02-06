package org.clulab.wm.eidos.system

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigValueFactory
import org.clulab.wm.eidos.EidosProcessor.EidosProcessor
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.CompactDomainOntology.CompactDomainOntologyBuilder
import org.clulab.wm.eidos.groundings.FastDomainOntology.FastDomainOntologyBuilder
import org.clulab.wm.eidos.groundings._
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.utils.Canonicalizer

import collection.JavaConverters._

class TestCachedDomainOntology extends Test {
  val ontologies: Iterable[String] = Seq("one", "two")
  val config: Config = ConfigFactory.load(EidosSystem.defaultConfig)
      .withValue("ontologies.useW2V", ConfigValueFactory.fromAnyRef(false, "Vectors are not necessary."))
      .withValue("ontologies.ontologies", ConfigValueFactory.fromIterable(Seq.empty[String].asJava, "Preloaded ontologies are not necessary."))
  val baseDir = "/org/clulab/wm/eidos/english/ontologies"
  val cacheDir: String = config[String]("ontologies.cacheDir")
  val reader: EidosSystem = new EidosSystem(config)
  val proc: EidosProcessor = reader.components.proc
  val canonicalizer = new Canonicalizer(reader.components.stopwordManager)
  val filter = true

  case class OntologySpec(abbrev: String, name: String, path: String)

  case class OntologyEntry(name: String, values: Array[String], patterns: Array[String]) {

    override def equals(other: Any): Boolean = {
      val that = other.asInstanceOf[OntologyEntry]
      val equal0 = true
      val equal1 = equal0 && this.name == that.name
      val equal2 = equal1 && this.values.zip(that.values).forall { case (left, right) => left == right }
      val equal3 = equal2 && this.patterns.zip(that.patterns).forall { case (left, right) => left == right }

      equal3
    }
  }

  protected def extract(domainOntology: DomainOntology): Seq[OntologyEntry]= {
    0.until(domainOntology.size).map { index =>
      OntologyEntry(
        domainOntology.getNamer(index).name,
        domainOntology.getValues(index).sorted,
        domainOntology.getPatterns(index).getOrElse(Array.empty).map(_.toString).sorted
      )
    }
    .sortBy(_.name)
  }

  protected def getDomainOntologies(ontologySpec: OntologySpec, includeParents: Boolean): Array[DomainOntology] = {
    val path = ontologySpec.path
    val domainOntologies = Array(false, true).flatMap { useCache =>
      println(s"Reading ${ontologySpec.abbrev} with includeParents = $includeParents and useCache = $useCache")

      if (!useCache) {
        val orig = DomainOntologies(baseDir + path, "", proc, canonicalizer, filter, useCache , includeParents)
        val copy =
          if (!includeParents)  new CompactDomainOntologyBuilder(orig.asInstanceOf[HalfTreeDomainOntology]).build()
          else new FastDomainOntologyBuilder(orig.asInstanceOf[FullTreeDomainOntology]).build()

        Array(orig, copy)
      }
      else {
        val cachePath = OntologyHandler.serializedPath(ontologySpec.abbrev, cacheDir, includeParents)

        Array(DomainOntologies("", cachePath, proc, canonicalizer, filter, useCache, includeParents))
      }
    }

    domainOntologies
  }

  protected def run(ontologySpec: OntologySpec, includeParents: Boolean): Unit = {
    val domainOntologies = getDomainOntologies(ontologySpec, includeParents).toSeq
    assert(domainOntologies.size == 3)
    val headOntologyEntries = extract(domainOntologies.head)

    domainOntologies.tail.foreach { domainOntology =>
      val ontologyEntries = extract(domainOntology)
      val names = ontologyEntries.map(_.name).distinct

      if (ontologyEntries.size != names.size)
        println("There are repeats!")

      if (headOntologyEntries.size != ontologyEntries.size)
        println("Oh no, the sizes are different!")
      else {
        headOntologyEntries.zip(ontologyEntries).foreach { case (expected, actual) =>
          if (expected != actual)
            println("It didn't work again!")
        }
      }
    }
  }

  def run(ontologySpec: OntologySpec): Unit = {
    Array(false, true).foreach { includeParents =>
      run(ontologySpec, includeParents)
    }
  }

  val ontologySpecs: Array[OntologySpec] = Array (
    //  OntologySpec("topo", "topoFlow ontology", "/topoflow_ontology.yml"),
    //  OntologySpec("mesh", "mesh ontology", "/mesh_ontology.yml"),
    //  OntologySpec("props", "props ontology", "/un_properties.yml"),

    OntologySpec("mitre12", "mitre12 ontology", "/mitre12_indicators.yml"),
    OntologySpec("un", "un ontology", "/un_ontology.yml"),
    OntologySpec("who", "un ontology", "/who_ontology.yml"),
    OntologySpec("wm", "wm ontology", "/wm_metadata.yml"),
    OntologySpec("wm_compositional", "wm compositional ontology", "/wm_compositional_metadata.yml"),
    OntologySpec("wm_flattened", "wm flattened ontology", "/wm_with_flattened_interventions_metadata.yml")
  )

//   ontologySpecs.foreach(run)
}
