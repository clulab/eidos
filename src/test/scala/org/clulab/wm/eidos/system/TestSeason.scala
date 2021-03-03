package org.clulab.wm.eidos.system

import java.time.LocalDateTime

import org.clulab.wm.eidos.attachments.Time
import org.clulab.wm.eidos.context.TimeStep
import org.clulab.wm.eidos.test.ExtractionTest

class TestSeason extends ExtractionTest {

  def seasons(text: String): Seq[Time] = extractMentions(text)
      .filter(_.label == "Time")
      .flatMap { m =>
        m.attachments.collect {
          case time: Time => time
        }
      }

  behavior of "season finder"

  longTimeNormTest should "identify a bi-gram (lean season) and a tri-gram (short rainy season) in one paragraph" in {
    // This should identify a bi-gram (lean season) and a tri-gram (short rainy season)
    // season expressions with date and location references in the same sentence.
    // It should also identify a modifier (early) for the bi-gram (lean season).
    val text =
    """
      In many areas of Afar, household cereal stocks
      are likely to be depleted by March 2018, prompting
      an early lean season after the annual short rainy season.
    """
    val earlyLeanSeason = TimeStep(
      LocalDateTime.of(2017, 12, 1, 0, 0),
      LocalDateTime.of(2018, 3, 1, 0, 0)
    )
    val shortRainySeason = TimeStep(
      LocalDateTime.of(2018, 4, 1, 0, 0),
      LocalDateTime.of(2018, 6, 1, 0, 0)
    )
    seasons(text).map(_.text) should contain allElementsOf Set("early lean season", "short rainy season")
    seasons(text).flatMap(_.interval.intervals) should contain allElementsOf Set(earlyLeanSeason, shortRainySeason)
  }

  longTimeNormTest should "identify a uni-gram season expression (meher) with date and location references in the previous sentence" in {
    val text =
    """
      In 2011, regions of Tigray have experienced a
      significant increase in the frequency of below-normal
      rains compared to previous years. In particular,
      the meher rains have experienced a 80 per cent increase.
    """
    val meher = TimeStep(
      LocalDateTime.of(2011, 9, 1, 0, 0),
      LocalDateTime.of(2012, 3, 1, 0, 0)
    )
    seasons(text).map(_.text) should contain("meher")
    seasons(text).flatMap(_.interval.intervals) should contain(meher)
  }

  longTimeNormTest should "not identify anything in a last paragraph" in {
    val text = """
        In agricultural regions of developing countries,
        it's known as the lean season that dangerous period
        between planting and harvesting when job opportunities
        are scarce and incomes plummet.
    """
    seasons(text) shouldBe empty
  }
}
