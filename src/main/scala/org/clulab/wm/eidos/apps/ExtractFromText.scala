package org.clulab.wm.eidos.apps

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.serialization.json.JsonUtils

object ExtractFromText extends App {
  val text = "Early rainfall improves harvests tremendously in (one) [two] {three} o(e t[o three." // args(0)
  val eidosSystem = new EidosSystem()
  val annotatedDocument = eidosSystem.extractFromText(text)
  val jldCorpus = new JLDCorpus(Seq(annotatedDocument))
  val jsonMentions = jldCorpus.serialize()
  val jsonld = JsonUtils.stringify(jsonMentions, pretty = true)

  println(jsonld)
}
