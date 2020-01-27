package ai.lum.eidos.restapp.models.utils

import org.json4s.JValue
import org.json4s.jackson.prettyJson
import org.json4s.jackson.parseJson

import play.api.libs.json.JsValue
import play.api.libs.json.{Json => JSon}

object JsonUtils {

   def toJsValue(jValue: JValue): JsValue = {
     val json = prettyJson(jValue)
     val jsValue = JSon.parse(json)

     jsValue
   }

  def toJValue(jsValue: JsValue): JValue = {
    val json = JSon.prettyPrint(jsValue)
    val jValue = parseJson(json)

    jValue
  }
}
