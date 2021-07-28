package org.clulab.wm.eidos.apps

import ai.lum.common.ConfigUtils._
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.DomainHandler

object ShowOntologyVersion extends App {
  val config = EidosSystem.defaultConfig

  {
    val ontologyPath = config[String]("ontologies.wm_compositional")
    val (versionOpt, _) = DomainHandler.getVersionOpt(ontologyPath)
    val version = versionOpt.getOrElse("unavailable")
    
    println(s"Compositional version: $version")
  }

  {
    val ontologyPath = config[String]("ontologies.wm_flattened")
    val (versionOpt, _) = DomainHandler.getVersionOpt(ontologyPath)
    val version = versionOpt.getOrElse("unavailable")

    println(s"    Flattened version: $version")
  }
}
