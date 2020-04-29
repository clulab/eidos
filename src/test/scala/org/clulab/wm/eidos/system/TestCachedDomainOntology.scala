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
  val config: Config = ConfigFactory.load(this.defaultConfig)
      .withValue("ontologies.useGrounding", ConfigValueFactory.fromAnyRef(false, "Vectors are not necessary."))
      .withValue("ontologies.ontologies", ConfigValueFactory.fromIterable(Seq.empty[String].asJava, "Preloaded ontologies are not necessary."))
  val baseDir = "/org/clulab/wm/eidos/english/ontologies"
  val cacheDir: String = config[String]("ontologies.cacheDir")
  val reader: EidosSystem = new EidosSystem(config)
  val proc: EidosProcessor = reader.components.proc
  val canonicalizer = new Canonicalizer(reader.components.stopwordManager, reader.components.proc.getTagSet)
  val filter = true

  case class OntologySpec(abbrev: String, path: String)

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
    val domainOntologies = Array(false, true).flatMap { useCacheForOntologies =>
      if (!useCacheForOntologies) {
        val orig = DomainOntologies(baseDir + path, "", proc, canonicalizer, filter, useCacheForOntologies , includeParents)
        val copy =
          if (!includeParents)  new CompactDomainOntologyBuilder(orig.asInstanceOf[HalfTreeDomainOntology]).build()
          else new FastDomainOntologyBuilder(orig.asInstanceOf[FullTreeDomainOntology]).build()

        Array(orig, copy)
      }
      else {
        val cachePath = OntologyHandler.serializedPath(ontologySpec.abbrev, cacheDir, includeParents)

        Array(DomainOntologies("", cachePath, proc, canonicalizer, filter, useCacheForOntologies, includeParents))
      }
    }

    domainOntologies
  }

  protected def test(ontologySpec: OntologySpec, includeParents: Boolean): Unit = {
    behavior of s"${ontologySpec.abbrev} with includeParents $includeParents"

    ignore should "Work in all three circumstances" in { // it
      val domainOntologies = getDomainOntologies(ontologySpec, includeParents).toSeq
      assert(domainOntologies.size == 3)

      val headOntologyEntries = extract(domainOntologies.head)
      val headNames = headOntologyEntries.map(_.name).distinct
      headOntologyEntries.size should be (headNames.size)

      domainOntologies.tail.foreach { domainOntology =>
        val ontologyEntries = extract(domainOntology)

        headOntologyEntries.size should be (ontologyEntries.size)
        headOntologyEntries.zip(ontologyEntries).foreach { case (expected, actual) =>
          expected should be (actual)
        }
      }
    }
  }

  def test(ontologySpec: OntologySpec): Unit = {
    Array(false, true).foreach { includeParents =>
      test(ontologySpec, includeParents)
    }
  }

  val ontologySpecs: Array[OntologySpec] = Array (
//    OntologySpec("topo",             "/topoflow_ontology.yml"),
//    OntologySpec("mesh",             "/mesh_ontology.yml"),
//    OntologySpec("props",            "/un_properties.yml"),

    OntologySpec("mitre12",          "/mitre12_indicators.yml"),
    OntologySpec("un",               "/un_ontology.yml"),
    OntologySpec("who",              "/who_ontology.yml"),
    OntologySpec("wm",               "/wm_metadata.yml"),
    OntologySpec("wm_compositional", "/wm_compositional_metadata.yml"),
    OntologySpec("wm_flattened",     "/wm_with_flattened_interventions_metadata.yml")
  )

   ontologySpecs.foreach(test)
}
