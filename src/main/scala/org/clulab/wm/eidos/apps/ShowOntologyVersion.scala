package org.clulab.wm.eidos.apps

import org.clulab.wm.eidos.EidosSystem
import ai.lum.common.ConfigUtils._
import org.clulab.wm.eidos.groundings.DomainHandler

object ShowOntologyVersion extends App {
  val config = EidosSystem.defaultConfig
  val wmCompositional = config[String]("ontologies.wm_compositional")
  val (versionOpt, _) = DomainHandler.getVersionOpt(wmCompositional)
  val version: String = versionOpt.getOrElse("unavailable")

  println(s"Version: $version")
}
