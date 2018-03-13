package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.mentions.EidosMention

case class OntologyGrounding(grounding: Seq[(String, Double)])

trait OntologyGrounder {
  def groundOntology(mention: EidosMention): OntologyGrounding
  def containsStopword(stopword: String): Boolean
}
