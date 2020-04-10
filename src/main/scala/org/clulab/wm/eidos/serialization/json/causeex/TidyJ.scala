package org.clulab.wm.eidos.serialization.json.causeex

import org.json4s._
import org.json4s.jackson.Serialization

trait TidyJ {
  def isTidy: Boolean
}

object TidyJ {
  val emptyJString: JString = JString("")
  val emptyJArray: JArray = JArray(List.empty)
  val emptyJSet: JSet = JSet(Set.empty)
  val emptyJObject: JObject = JObject()

  def isTidy(jField: JField): Boolean = isTidy(jField._2)

  def isTidy(jValue: JValue): Boolean = jValue match {
    case value: TidyJObject => value.isTidy
    case value: TidyJArray => value.isTidy

    case null => false
    case JNull => false
    case JNothing => false
    case value: JString => value.values.nonEmpty
    case value: JArray => value.values.nonEmpty
    case value: JSet => value.values.nonEmpty
    case value: JObject => value.values.nonEmpty
    case _ => true
  }

  def serialize(jValue: JValue): String = {
    implicit val formats: Formats = DefaultFormats.preservingEmptyValues

    Serialization.writePretty(jValue)
  }
}

class TidyJObject(protected var jFields: List[JField], val required: Boolean)
    extends JObject(TidyJObject.tidy(jFields)) with TidyJ {
  def +(other: TidyJObject) = new TidyJObject(jFields ++ other.jFields, required)

  def isTidy: Boolean = {
    required || jFields.exists(TidyJ.isTidy)
  }

  def serialize: String = TidyJ.serialize(this)
}

object TidyJObject {

  def apply(jFields: JField*)(implicit required: Boolean = false) = new TidyJObject(jFields.toList, required)

  def empty(required: Boolean = false) = new TidyJObject(List.empty[JField], required)

  def tidy(jFields: List[JField]): List[JField] = jFields.filter(TidyJ.isTidy)
}

class TidyJArray(protected var jValues: List[JValue], val required: Boolean)
    extends JArray(TidyJArray.tidy(jValues)) with TidyJ {
  def +(other: TidyJArray) = new TidyJArray(jValues ++ other.jValues, required)

  def isTidy: Boolean = {
    required || jValues.exists(TidyJ.isTidy)
  }

  def serialize: String = TidyJ.serialize(this)
}

object TidyJArray {

  // It's necessary to change the name in order to have multiple overloaded methods with default arguments.
  def items(jValues: JValue*)(implicit required: Boolean = false) = new TidyJArray(jValues.toList, required)

  def apply(jValues: List[JValue])(implicit required: Boolean = false) = new TidyJArray(jValues, required)

  def empty(required: Boolean = false) = new TidyJArray(List.empty[JValue], required)

  def tidy(jValues: List[JValue]): List[JValue] = jValues.filter(TidyJ.isTidy)
}