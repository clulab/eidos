package org.clulab.wm

import CAG._
import TestUtils._

import org.clulab.odin.Attachment
import org.clulab.odin.Mention

class TestCagP0 extends AgroTest {
  val mentions = extractMentions(p0s1)

  "p0s1" should "have the correct triples" in {
    // Should this just be "rainfall"?
    val rainfall = newTextMentionSpec("in rainfall", newDecrease("decrease"))
    val poverty = newTextMentionSpec("poverty", newIncrease("increased", "significantly"))
    
    val rainfallCausedPoverty = newCauseEffectSpec(rainfall, poverty)

    // Add unattached nodes and test
    // Also check them for lack of connections
    // Argument to indicate number of expected attachments?
    // Even if attached, don't want extra attachments
    // newOrphanSpec?
    
    rainfallCausedPoverty.test(mentions) should be ((true, true, true))    
  }
}
