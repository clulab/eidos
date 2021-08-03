package org.clulab.wm.eidos.apps

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.clulab.utils.Configured

trait EidosConfigured extends Configured {
  // This is used instead of load so that no default references or default overrides are involved.
  // In other words, the values you are looking for had better be in this file (resource).
  // This line doesn't work if there is a leading / in the resource name.  I tried.
  lazy val config = ConfigFactory.parseResourcesAnySyntax("org/clulab/wm/eidos/apps")

  override def getConf: Config = config
}

trait EidosApp extends App with EidosConfigured
