package org.clulab.wm.eidos.serialization.json.causex

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.Time
import org.clulab.wm.eidos.context.TimEx
import org.clulab.wm.eidos.context.TimeStep
import org.clulab.wm.eidos.serialization.json.causeex.SimpleTime
import org.clulab.wm.eidos.serialization.json.causeex.TidyJ
import org.clulab.wm.eidos.serialization.json.causeex.TidyJObject
import org.clulab.wm.eidos.serialization.json.causeex.TidyJArray
import org.clulab.wm.eidos.test.TestUtils.ExtractionTest
import org.json4s._

class TestCauseExSerializer extends ExtractionTest {
  val optionalEmptyTidyJObject: TidyJObject = TidyJObject()(required = false)
  val requiredEmptyTidyJObject: TidyJObject = TidyJObject()(required = true)

  behavior of "LocalDateTime"

  it should "reproduce nanosecond results" in {
    val today = LocalDateTime.now()
    val tomorrow = today.plusDays(1L)

    today.getNano should be (tomorrow.getNano)
  }

  behavior of "SimpleTime"

  it should "work on a simple example" in {
    val today = LocalDateTime.now
    val tomorrow = today.plusDays(1L)
    val yesterday = today.minusDays(1L)
    val time = Time(TimEx(Interval(0, 1), Seq(TimeStep(today, tomorrow), TimeStep(yesterday, today)), "text"))
    val simpleTime = SimpleTime.fromTime(time)

    simpleTime.start should be (yesterday)
    simpleTime.end should be (tomorrow)
    simpleTime.duration shouldBe defined
    simpleTime.duration.get should be (ChronoUnit.MILLIS.between(yesterday, tomorrow))
  }

  behavior of "TidyJObject"

  it should "keep required values, even if empty" in {
    val tidyJObject = TidyJObject(
      "null" -> null,
      "JNull" -> JNull,
      "JNothing" -> JNothing,
      "emptyJString" -> TidyJ.emptyJString,
      "emptyJArray" -> TidyJ.emptyJArray,
      "emptyJSet" -> TidyJ.emptyJSet,
      "emptyJObject" -> TidyJ.emptyJObject,
      "optionalEmptyTidyJObject" -> TidyJObject.empty(required = false),
      "requiredEmptyTidyJObject" -> TidyJObject.empty(required = true),
      "optionalEmptyTidyJArray" -> TidyJArray.empty(required = false),
      "requiredEmptyTidyJArray" -> TidyJArray.empty(required = true)
    )
    val json = tidyJObject.serialize

    // It should fit on four lines.
    json.count(_ == '\n') should be (3)
    json should include ("requiredEmptyTidyJObject")
    json should include ("requiredEmptyTidyJArray")
  }

  behavior of "TidyJArray"

  it should "keep required values, even if empty" in {
    val tidyJArray = TidyJArray.items(
      null,
      JNull,
      JNothing,
      TidyJ.emptyJString,
      TidyJ.emptyJArray,
      TidyJ.emptyJSet,
      TidyJ.emptyJObject,
      TidyJObject.empty(required = false),
      TidyJObject.empty(required = true),
      TidyJArray.empty(required = false),
      TidyJArray.empty(required = true)
    )(required = true)

    val json = tidyJArray.serialize

    // It should fit on four lines.
    json.count(_ == '\n') should be (0)
    json should include ("{ }")
    json should include ("[ ]")
  }
}
