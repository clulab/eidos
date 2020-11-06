package org.clulab.wm.eidos.system

import java.time.LocalDateTime

import org.clulab.wm.eidos.attachments.Time
import org.clulab.wm.eidos.context.TimeStep
import org.clulab.wm.eidos.test.TestUtils._


class TestSeason extends ExtractionTest {

  def seasons(text: String): Seq[Time] = extractMentions(text).filter(_.label == "Time").flatMap(m => m.attachments.collect{
    case time: Time => time
  })

  it should "identify seasons" in {
    // This should identify bi-grams (lean season) and tri-grams (short rainy season)
    // season expressions with date and location references in the same sentence.
    val text = """
      In many areas of Afar, household cereal stocks
      are likely to be depleted by March 2018, prompting
      an early lean season after the annual short rainy season.
    """
    val leanSeason = TimeStep(
      LocalDateTime.of(2018, 1, 1, 0, 0),
      LocalDateTime.of(2018, 4, 1, 0, 0)
    )
    val shortRainySeason = TimeStep(
      LocalDateTime.of(2018, 4, 1, 0, 0),
      LocalDateTime.of(2018, 6, 1, 0, 0)
    )
    seasons(text).map(_.text) should contain allElementsOf Set("lean season", "short rainy season")
    seasons(text).flatMap(_.interval.intervals) should contain allElementsOf Set(leanSeason, shortRainySeason)


    // This should identify a uni-gram season expression (meher)
    // with date and location references in the previous sentence.
    val text2 ="""
      In 2011, regions of Tigray have experienced a
      significant increase in the frequency of below-normal
      rains compared to previous years. In particular,
      the meher rains have experienced a 80 per cent increase.
    """
    val meher = TimeStep(
      LocalDateTime.of(2011, 6, 1, 0, 0),
      LocalDateTime.of(2011, 10, 1, 0, 0)
    )
    seasons(text2).map(_.text) should contain ("meher")
    seasons(text2).flatMap(_.interval.intervals) should contain (meher)


    // This should not identify anything.
    val text3 = """
        In agricultural regions of developing countries,
        it's known as the lean season that dangerous period
        between planting and harvesting when job opportunities
        are scarce and incomes plummet.
    """
    seasons(text3) shouldBe empty
  }
}