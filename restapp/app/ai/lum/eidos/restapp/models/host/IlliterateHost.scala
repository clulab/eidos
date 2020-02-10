package ai.lum.eidos.restapp.models.host

import ai.lum.eidos.restapp.models.text.EidosText

import org.json4s.JValue

class IlliterateHost(prime: Boolean) extends EidosHost {

  def process(eidosText: EidosText): JValue = {
    Thread.sleep(5000)
    eidosText.toJson
  }
}
