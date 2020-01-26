package ai.lum.eidos.host

import ai.lum.eidos.text.EidosText
import org.json4s.JValue
import org.json4s.JsonAST.JNull

class IlliterateHost(prime: Boolean) extends EidosHost {

  def process(eidosText: EidosText): JValue = {
    // Sleep awhile
    // Read some default file with a pre-calculated answer
    // Return it
    JNull
  }
}
