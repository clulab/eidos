package org.clulab.wm.eidos.serialization.json.causex

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.Time
import org.clulab.wm.eidos.context.TimEx
import org.clulab.wm.eidos.context.TimeStep
import org.clulab.wm.eidos.serialization.json.JsonUtils
import org.clulab.wm.eidos.serialization.json.causeex.SimpleTime
import org.clulab.wm.eidos.serialization.json.causeex.TidyJObject
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
      "emptyJString" -> TidyJObject.emptyJString,
      "emptyJArray" -> TidyJObject.emptyJArray,
      "emptyJSet" -> TidyJObject.emptyJSet,
      "emptyJObject" -> TidyJObject.emptyJObject,
      "optionalEmptyTidyJObject" -> TidyJObject()(required = false),
      "requiredEmptyTidyJObject" -> TidyJObject()(required = true)
    )
    val json = JsonUtils.stringify(tidyJObject, pretty = true)

    // It should fit on three lines.
    json.count(_ == '\n') should be (2)
    json should include ("requiredEmptyTidyJObject")
  }
}
