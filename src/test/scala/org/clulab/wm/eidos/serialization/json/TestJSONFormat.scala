package org.clulab.wm.eidos.serialization.json

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidos.utils.PlayUtils
import org.json4s.JsonAST.JValue
import org.json4s._
import org.json4s.JsonDSL._

class TestJSONFormat extends Test {

  def hasUndesired(text: String): Boolean = {
    val line = text.replace('\n', ' ').replace('\r', ' ')

    line.matches(".*(blank|absence|null|nothing|empty).*")
  }

  def isUndesired(jValue: JValue): Boolean = {

  }
  
  def clean(map: Map[String, JValue]): JObject = {
    val cleaned: List[(String, JValue)] = map.toList.filter { case (_, value) =>
      value match {
        case JNull => false
        case JNothing => false
        case value: JString => value != null && value.values.size > 0
        case value: JArray => value != null && value.values.size > 0
        case value: JSet => value != null && value.values.size > 0
        case value => value != null
      }
    }
    val fielded: List[JField] = cleaned.map { case (key, value) => new JField(key, value)}

    new JObject(fielded)
  }

  // Check first to see if dirty and if not, return original object
  // Can this be made recursive?
  // isUndesired?


  val jObject: JObject = {
    val full = List[Int](1, 2, 3, 4)
    val empty = List.empty[Int]
    val something = Option[String]("hello")
    val nothing = Option[String](null)

    val result: JObject = clean(Map[String, JValue](
        ("string" -> "Hello, world!"),
        ("blank" -> ""),
        ("absence" -> null), // An actual null would heretofore crash the webapp.
        ("full" -> full),
        ("empty" -> empty),
        ("something" -> something),
        ("nothing" -> nothing)
    ))
    result
  }

  behavior of "pretty string"

  it should "not record nulls, Nones, or Emptys" in {
    val prettyString = stringify(jObject, pretty = true)

    println("pretty = " + prettyString)
    hasUndesired(prettyString) should be (false)
  }

  behavior of "ugly string"

  it should "not record nulls, Nones, or Emptys" in {
    val uglyString = stringify(jObject, pretty = false)

    println("ugly = " + uglyString)
    hasUndesired(uglyString) should be (false)
  }

  behavior of "webapp string"

  it should "not record nulls, Nones, or Emptys" in {
    val webappString = PlayUtils.toPlayJson(jObject).toString

    println("webapp = " + webappString)
    hasUndesired(webappString) should be (false)
  }
}
