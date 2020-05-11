package org.clulab.wm.eidos.serialization.json

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.serialization.utils.TidyJObject
import org.clulab.wm.eidos.test.TestUtils.Test
import org.clulab.wm.eidos.utils.PlayUtils
import org.json4s.{JField, JObject}
import org.json4s.JsonDSL._


class TestJSONFormat extends Test {
  private val occupiedList = List[Int](1, 2, 3, 4)
  private val desertedList = List.empty[Int]
  private val occupiedSet = Set[Int](1, 2, 3, 4)
  private val desertedSet = Set.empty[Int]
  private val something = Option[String]("hello")
  private val nothing = Option[String](null)
  private val sloppyRecursive = List("", null)
  private val tidyRecursive = TidyJObject(List(
    "first" -> "",
    "last" -> null
  ))

  val sloppyJObject: JObject =
    ("string" -> "Hello, world!") ~
        ("blank" -> "") ~
        // ("absence" -> null) ~
        ("occupiedList" -> occupiedList) ~
        ("desertedList" -> desertedList) ~
        ("occupiedSet" -> occupiedSet) ~
        ("desertedSet" -> desertedSet) ~
        ("something" -> something) ~
        ("nothing" -> nothing) ~
        ("sloppyRecursive" -> sloppyRecursive)

  val tidyJObject: TidyJObject = TidyJObject(List(
    "string" -> "Hello, world!",
    "blank" -> "",
    "absence" -> null,
    "occupiedList" -> occupiedList,
    "desertedList" -> desertedList,
    "occupiedSet" -> occupiedSet,
    "desertedSet" -> desertedSet,
    "something" -> something,
    "nothing" -> nothing,
    "tidyRecursive" -> tidyRecursive
  ))

  def hasDirtyField(text: String): Boolean = {
    val line = text.replace('\n', ' ').replace('\r', ' ')

    line.matches(".*(blank|absence|null|nothing|desertedList).*")
  }

  behavior of "pretty string"

  it should "not record nulls, Nones, or Emptys" in {
    val sloppyString = stringify(sloppyJObject, pretty = true)
    val tidyString = stringify(tidyJObject, pretty = true)

//    println("pretty sloppy = " + sloppyString)
//    println("pretty tidy = " + tidyString)

    hasDirtyField(sloppyString) should be (true)
    hasDirtyField(tidyString) should be (false)
  }

  behavior of "ugly string"

  it should "not record nulls, Nones, or Emptys" in {
    val sloppyString = stringify(sloppyJObject, pretty = false)
    val tidyString = stringify(tidyJObject, pretty = false)

//    println("ugly sloppy = " + sloppyString)
//    println("ugly tidy = " + tidyString)

    hasDirtyField(sloppyString) should be (true)
    hasDirtyField(tidyString) should be (false)
  }

  behavior of "webapp string"

  it should "not record nulls, Nones, or Emptys" in {
    val sloppyString = PlayUtils.toPlayJson(sloppyJObject).toString
    val tidyString = PlayUtils.toPlayJson(tidyJObject).toString

//    println("webapp sloppy = " + sloppyString)
//    println("webapp tidy = " + tidyString)

    hasDirtyField(sloppyString) should be (true)
    hasDirtyField(tidyString) should be (false)
  }
}
