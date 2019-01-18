package org.clulab.wm.eidos.serialization.json

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidos.utils.PlayUtils

import org.json4s.{JField, JObject}
import org.json4s.JsonDSL._


class TestJSONFormat extends Test {
  private val occupied = List[Int](1, 2, 3, 4)
  private val deserted = List.empty[Int]
  private val something = Option[String]("hello")
  private val nothing = Option[String](null)
  private val recursive = List("", "", "")

  val sloppyJObject: JObject =
    ("string" -> "Hello, world!") ~
        ("blank" -> "") ~
        // ("absence" -> null) ~
        ("occupied" -> occupied) ~
        ("deserted" -> deserted) ~
        ("something" -> something) ~
        ("nothing" -> nothing) ~
        ("recursive" -> recursive)

  val tidyJObject: TidyJObject = TidyJObject(List(
    "string" -> "Hello, world!",
    "blank" -> "",
    "absence" -> null,
    "occupied" -> occupied,
    "deserted" -> deserted,
    "something" -> something,
    "nothing" -> nothing,
    "recursive" -> TidyJObject(recursive.zipWithIndex.map { case(value, index) => new JField(index.toString, value) })
  ))

  def hasDirtyField(text: String): Boolean = {
    val line = text.replace('\n', ' ').replace('\r', ' ')

    line.matches(".*(blank|absence|null|nothing|deserted).*")
  }

  behavior of "pretty string"

  it should "not record nulls, Nones, or Emptys" in {
    val sloppyString = stringify(sloppyJObject, pretty = true)
    val tidyString = stringify(tidyJObject, pretty = true)

    println("pretty sloppy = " + sloppyString)
    println("pretty tidy = " + tidyString)

    hasDirtyField(sloppyString) should be (true)
    hasDirtyField(tidyString) should be (false)
  }

  behavior of "ugly string"

  it should "not record nulls, Nones, or Emptys" in {
    val sloppyString = stringify(sloppyJObject, pretty = false)
    val tidyString = stringify(tidyJObject, pretty = false)

    println("ugly sloppy = " + sloppyString)
    println("ugly tidy = " + tidyString)

    hasDirtyField(sloppyString) should be (true)
    hasDirtyField(tidyString) should be (false)
  }

  behavior of "webapp string"

  it should "not record nulls, Nones, or Emptys" in {
    val sloppyString = PlayUtils.toPlayJson(sloppyJObject).toString
    val tidyString = PlayUtils.toPlayJson(tidyJObject).toString

    println("webapp sloppy = " + sloppyString)
    println("webapp tidy = " + tidyString)

    hasDirtyField(sloppyString) should be (true)
    hasDirtyField(tidyString) should be (false)
  }
}
