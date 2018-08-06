package org.clulab.wm.eidos.apps

import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.EidosOntologyGrounder.{FAO_NAMESPACE, UN_NAMESPACE, WDI_NAMESPACE}
import org.clulab.wm.eidos.groundings.{DomainOntology, FAOOntology, UNOntology, WDIOntology}

object CacheOntologies extends App {

  val config = ConfigFactory.load("eidos")
      .withValue("EidosSystem.useW2V", ConfigValueFactory.fromAnyRef(false, "Don't use vectors when caching ontologies."))
  val reader = new EidosSystem(config)
  val loadableAttributes = reader.LoadableAttributes

  val ontologies: Seq[String] = loadableAttributes.ontologies
  if (ontologies.isEmpty)
    throw new RuntimeException("No ontologies were specified, please check the config file.")

  val absoluteCachedDir: String = loadableAttributes.cachedOntologiesDir
  val proc = reader.proc

  val domainOntologies = ontologies.map {
    _ match {
      case name @ UN_NAMESPACE  =>  UNOntology(name, loadableAttributes.unOntologyPath,  absoluteCachedDir, proc, loadSerialized = false)
      case name @ WDI_NAMESPACE => WDIOntology(name, loadableAttributes.wdiOntologyPath, absoluteCachedDir, proc, loadSerialized = false)
      case name @ FAO_NAMESPACE => FAOOntology(name, loadableAttributes.faoOntologyPath, absoluteCachedDir, proc, loadSerialized = false)
      case name @ _ => throw new IllegalArgumentException("Ontology " + name + " is not recognized.")
    }
  }

  println(s"Saving ontologies to $absoluteCachedDir...")
  domainOntologies.foreach(ont => ont.save(DomainOntology.serializedPath(ont.name, absoluteCachedDir)))
  println(s"Finished serializing ${domainOntologies.length} ontologies.")
}
