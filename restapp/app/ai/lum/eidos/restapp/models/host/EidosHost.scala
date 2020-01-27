package ai.lum.eidos.restapp.models.host

import ai.lum.eidos.restapp.models.text.EidosText
import org.json4s.JsonAST.JValue

trait EidosHost {
  def process(eidosText: EidosText): JValue
}
