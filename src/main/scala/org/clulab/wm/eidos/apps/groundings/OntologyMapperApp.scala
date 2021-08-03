package org.clulab.wm.eidos.apps.groundings

import org.clulab.wm.eidos.EidosConfigured
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.groundings.OntologyMapper

object OntologyMapperApp extends EidosConfigured {

  // All of this and the call to mapIndicators is usually arranged in CacheOntologies.
  def main(args: Array[String]): Unit = {
    val reader = new EidosSystem()

    OntologyMapper.mapIndicators(reader)
  }
}
