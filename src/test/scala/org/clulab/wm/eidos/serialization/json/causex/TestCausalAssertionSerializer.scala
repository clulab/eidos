package org.clulab.wm.eidos.serialization.json.causex

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

import org.clulab.struct.Interval
import org.clulab.wm.eidos.attachments.Time
import org.clulab.wm.eidos.context.TimEx
import org.clulab.wm.eidos.context.TimeStep
import org.clulab.wm.eidos.serialization.json.causeex.CausalAssertionSerializer
import org.clulab.wm.eidos.test.TestUtils.ExtractionTest

class TestCausalAssertionSerializer extends ExtractionTest {

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
    val simpleTime = CausalAssertionSerializer.timeToSimpleTime(time)

    simpleTime.start should be (yesterday)
    simpleTime.end should be (tomorrow)
    simpleTime.duration shouldBe defined
    simpleTime.duration.get should be (ChronoUnit.MILLIS.between(yesterday, tomorrow))
  }
}
