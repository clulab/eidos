package org.clulab.wm.eidos.apps.extract

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.JsonUtils
import org.clulab.wm.eidos.serialization.jsonld.JLDCorpus

object ExtractFromText extends App {
//  val text = "The price of oil decreased water transportation." // args(0)
  val text = "Attempts at stabilizing prices are rarely completely successful because they need to be combined with safety nets and other social protection measures to mitigate the impact of higher food prices and to help prevent violent conflicts ."
  val eidosSystem = new EidosSystem()
  val annotatedDocument = eidosSystem.extractFromText(text)
  val jldCorpus = new JLDCorpus(Seq(annotatedDocument))
  val jCorpus = jldCorpus.serialize()
  val jsonld = JsonUtils.stringify(jCorpus, pretty = true)

  println(jsonld)
}
