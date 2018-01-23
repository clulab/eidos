package org.clulab.wm

import CAG._
import TestUtils._

import org.clulab.odin.Attachment
import org.clulab.odin.Mention

class TestCagP1 extends AgroTest {
  val mentions = extractMentions(p1s1)
  "p1s1" should "have correct nodes" in {
    val poorRainfall = newTextMentionSpec("poor rainfall in southeastern areas", newQuantification("poor"))
    val cerealProduction = newTextMentionSpec("cereal production", newQuantification("low"), newDecrease("low"))
    
    poorRainfall.test(mentions) should be (true)
    cerealProduction.test(mentions) should be (true)
  }
}
