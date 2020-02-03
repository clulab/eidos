package ai.lum.eidos.restapp.models.host

import ai.lum.eidos.restapp.models.text.EidosText
import org.json4s.JValue
import org.json4s.JNull
import org.json4s.JString

class IlliterateHost(prime: Boolean) extends EidosHost {

  def process(eidosText: EidosText): JValue = {
    // Sleep awhile
    // Read some default file with a pre-calculated answer
    // Return it
    Thread.sleep(5000)
    JString(eidosText.getText)
  }
}
