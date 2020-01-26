package ai.lum.eidos.host

import ai.lum.eidos.text.EidosText
import org.json4s.JsonAST.JValue

trait EidosHost {
  def process(eidosText: EidosText): JValue
}
