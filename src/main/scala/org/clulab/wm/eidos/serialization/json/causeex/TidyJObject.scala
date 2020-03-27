package org.clulab.wm.eidos.serialization.json.causeex

import org.json4s._

class TidyJObject(protected var jFields: List[JField], val required: Boolean) extends JObject(TidyJObject.tidy(jFields)) {
  def +(other: TidyJObject) = new TidyJObject(jFields ++ other.jFields, required)
}

object TidyJObject {

  def isTidy(keyAndValue: (String, JValue)): Boolean = keyAndValue._2 match {
    case null => false
    case JNull => false
    case JNothing => false
    case value: JString => value.values.nonEmpty
    case value: JArray => value.values.nonEmpty
    case value: JSet => value.values.nonEmpty
    case value: TidyJObject => value.required || value.jFields.exists(isTidy)
    case _ => true
  }

  def tidy(jFields: List[JField]): List[JField] = jFields.filter(isTidy)

  def apply(jFields: JField*)(implicit required: Boolean = false) = new TidyJObject(jFields.toList, required)

  def apply() = new TidyJObject(List.empty[JField], false)
}