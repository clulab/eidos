package org.clulab.wm.eidos.serialization.json

import org.json4s.JValue

import org.clulab.serialization.json.{stringify => jsonStringify}

object JsonUtils {

  def stringify(json: JValue, pretty: Boolean): String = jsonStringify(json, pretty)
}
