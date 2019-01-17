package org.clulab.wm.eidos.serialization.json

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidos.utils.PlayUtils

import org.json4s.JObject
import org.json4s.JsonDSL._


class TestJSONFormat extends Test {
  val occupied = List[Int](1, 2, 3, 4)
  val deserted = List.empty[Int]
  val something = Option[String]("hello")
  val nothing = Option[String](null)

  val sloppyJObjectList: JObject =
    ("string" -> "Hello, world!") ~
        ("blank" -> "") ~
//        ("absence" -> null) ~
        ("occupied" -> occupied) ~
        ("deserted" -> deserted) ~
        ("something" -> something) ~
        ("nothing" -> nothing)

  val sloppyJObject: JObject = sloppyJObjectList

  val tidyJObject: TidyJObject = new TidyJObject(List(
    ("string" -> "Hello, world!"),
    ("blank" -> ""),
    ("absence" -> null),
    ("occupied" -> occupied),
    ("deserted" -> deserted),
    ("something" -> something),
    ("nothing" -> nothing))
  )

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
