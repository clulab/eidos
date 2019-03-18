package org.clulab.wm.eidos.serialization.json

import org.json4s._

class TidyJObject(protected var jFields: List[JField]) extends JObject(TidyJObject.tidy(jFields)) {
  def +(other: TidyJObject) = new TidyJObject(jFields ++ other.jFields)
}

object TidyJObject {

  def isTidy(keyAndValue: (String, JValue)): Boolean = keyAndValue._2 match {
    case null => false
    case JNull => false
    case JNothing => false
    case value: JString => value.values.size > 0
    case value: JArray => value.values.size > 0
    case value: JSet => value.values.size > 0
    case value: TidyJObject => value.jFields.exists(isTidy)
    case _ => true
  }

  def tidy(jFields: List[JField]): List[JField] = jFields.filter(isTidy)

  def apply(jFields: List[JField]) = new TidyJObject(jFields)

  def apply() = new TidyJObject(List.empty[JField])
}