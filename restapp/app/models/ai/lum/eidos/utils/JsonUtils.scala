package models.ai.lum.eidos.utils

import org.json4s.JValue

import play.api.libs.json.JsValue
import play.api.libs.json.{Json => JSon}

import org.json4s.jackson.prettyJson
import org.json4s.jackson.parseJson

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
