package org.clulab.wm.eidos.utils

import play.api.libs.{ json => pjson }
import org.{ json4s => j4s }

object PlayUtils {

  implicit def toJson4s(json: play.api.libs.json.JsValue):org.json4s.JValue = json match {
    case pjson.JsString(str) => j4s.JString(str)
    case pjson.JsNull => j4s.JNull
    case pjson.JsBoolean(value) => j4s.JBool(value)
    case pjson.JsNumber(value) => j4s.JDecimal(value)
    case pjson.JsArray(items) => j4s.JArray(items.map(toJson4s).toList)
    case pjson.JsObject(items) => j4s.JObject(items.map { case (k, v) => k -> toJson4s(v)}.toList)
  }

  implicit def toPlayJson(json: org.json4s.JValue): play.api.libs.json.JsValue = json match {
    case j4s.JString(str) => pjson.JsString(str)
    case j4s.JNothing => pjson.JsNull
    case j4s.JNull => pjson.JsNull
    case j4s.JDecimal(value) => pjson.JsNumber(value)
    case j4s.JDouble(value) => pjson.JsNumber(value)
    case j4s.JInt(value) => pjson.JsNumber(BigDecimal(value))
    case j4s.JLong(value) => pjson.JsNumber(BigDecimal(value))
    case j4s.JBool(value) => pjson.JsBoolean(value)
    case j4s.JArray(fields) => pjson.JsArray(fields.map(toPlayJson))
    case j4s.JObject(fields) => pjson.JsObject(fields.map { case (k, v) => k -> toPlayJson(v)}.toMap)
    case j4s.JSet(fields) => pjson.JsArray(fields.toList.map(toPlayJson))
  }
}