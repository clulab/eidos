package org.clulab.wm.eidos.serialization.json

import org.json4s.JValue
import org.clulab.serialization.json.{stringify => jsonStringify}
import org.json4s.JsonInput
import org.json4s.jackson.JsonMethods.{parse => jsonParse}

object JsonUtils {

  def stringify(json: JValue, pretty: Boolean = true): String = jsonStringify(json, pretty)

  def parse(in: JsonInput): JValue = jsonParse(in)
}
