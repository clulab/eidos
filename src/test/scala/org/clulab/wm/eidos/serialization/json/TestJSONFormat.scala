package org.clulab.wm.eidos.serialization.json

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidos.utils.PlayUtils
import org.json4s.JsonAST.JValue
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

class TestJSONFormat extends Test {

  def hasUndesired(text: String): Boolean = {
    val line = text.replace('\n', ' ').replace('\r', ' ')

    line.matches(".*(null|nothing|empty).*")
  }

  // This method is taken from JLDSerializer.
  def noneIfEmpty(values: Seq[JValue]): Option[Seq[JValue]] =
    if (values.isEmpty) None
    else Some(values)

  // This method is taken from JLDSerializer.
  def toJObjects(jldObjects: Seq[JLDObject]): Option[Seq[JValue]] =
    noneIfEmpty(jldObjects.map(_.toJObject).toList)

  val jObject: JObject = {
    val full = List[Int](1, 2, 3, 4)
    val empty = List.empty[Int]
    val something: JValue = Option[String]("hello")
    val nothing: JValue = Option[String](null)

    val safeFull = noneIfEmpty(full.map(new JInt(_)))
    val safeEmpty = noneIfEmpty(empty.map(new JInt(_)))

    ("string" -> "Hello, world!") ~
        // ("absence" -> null) ~ // An actual null will crash the webapp.
        ("full" -> safeFull) ~
        ("empty" -> safeEmpty) ~
        ("something" -> something) ~
        ("nothing" -> nothing)
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
