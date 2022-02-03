package org.clulab.wm.wmexchanger2.utils

import org.clulab.wm.eidos.groundings.OntologyHandler
import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.wmexchanger.utils.Extensions
import org.clulab.wm.wmexchanger2.wmeidos.EidosSystemish

import java.io.File
import scala.collection.mutable

class OntologyMap(eidosSystem: EidosSystemish, ontologyDir: String) {
  protected val map: mutable.Map[String, OntologyHandler] = mutable.Map.empty

  def apply(ontologyId: String): OntologyHandler = synchronized {
    map.getOrElse(ontologyId, {
      val ontologyFile = FileEditor(new File(ontologyId)).setExt(Extensions.yml).setDir(ontologyDir).get
      val ontologyHandler = eidosSystem.newOntologyHandler(ontologyFile)

      map += ontologyId -> ontologyHandler
      ontologyHandler
    })
  }
}
