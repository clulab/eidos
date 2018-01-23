package org.clulab.wm

import CAG._
import TestUtils._

import org.clulab.odin.Attachment
import org.clulab.odin.Mention

class TestCagP0 extends AgroTest {
  val mentions = extractMentions(p0s1)

  "p0s1" should "have the correct triples" in {
    // Should this just be "rainfall"?
    val rainfall = newNodeSpec("in rainfall", newDecrease("decrease"))
    val poverty = newNodeSpec("poverty", newIncrease("increased", "significantly"))
    
    val rainfallCausedPoverty = newEdgeSpec(rainfall, poverty)
    
    rainfallCausedPoverty.test(mentions) should be (true)    
  }
}
