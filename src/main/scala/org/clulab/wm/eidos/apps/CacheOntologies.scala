package org.clulab.wm.eidos.apps

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.utils.Configured
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.EidosOntologyGrounder.{FAO_NAMESPACE, UN_NAMESPACE, WDI_NAMESPACE}
import org.clulab.wm.eidos.groundings.{DomainOntology, FAOOntology, UNOntology, WDIOntology}
import org.clulab.wm.eidos.utils.FileUtils.findFiles
import org.clulab.wm.eidos.utils.Sourcer

object CacheOntologies extends App with Configured {

  val config = ConfigFactory.load("eidos")
  override def getConf: Config = config

  val reader = new EidosSystem()
  val proc = reader.proc

  val unOntologyPath: String = getArgString("EidosSystem.unOntologyPath", Option("/org/clulab/wm/eidos/ontologies/un_ontology.yml"))
  val wdiOntologyPath: String = getArgString("EidosSystem.wdiOntologyPath", Option("/org/clulab/wm/eidos/ontologies/wdi_ontology.yml"))
  val faoOntologyPath: String = getArgString("EidosSystem.faoOntology", Option("/org/clulab/wm/eidos/ontologies/fao_variable_ontology.yml"))
  val absoluteCachedDir: String = getArgString("EidosSystem.cachedOntologiesDir", Option("/org/clulab/wm/eidos/ontologies/cached/"))

  // These are needed to construct some of the loadable attributes even though it isn't a path itself.
  def ontologies: Seq[String] = getArgStrings("EidosSystem.ontologies", Some(Seq.empty))
  val domainOntologies = if (ontologies.isEmpty) {
    throw new RuntimeException("No ontologies were specified, please check the config file.")
  } else {
    ontologies.map {
      _ match {
        case name @ UN_NAMESPACE  =>  UNOntology(name,  unOntologyPath, absoluteCachedDir, proc, loadSerialized = false)
        case name @ WDI_NAMESPACE => WDIOntology(name, wdiOntologyPath, absoluteCachedDir, proc, loadSerialized = false)
        case name @ FAO_NAMESPACE => FAOOntology(name, faoOntologyPath, absoluteCachedDir, proc, loadSerialized = false)
        case name @ _ => throw new IllegalArgumentException("Ontology " + name + " is not recognized.")
      }
    }
  }

  println(s"Saving ontologies to $absoluteCachedDir...")
  domainOntologies.foreach(ont => ont.save(DomainOntology.serializedPath(ont.name, absoluteCachedDir)))
  println(s"Finished serializing ${domainOntologies.length} ontologies.")

}
