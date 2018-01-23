package org.clulab.wm

import CAG._
import TestUtils._

import org.clulab.odin.Attachment
import org.clulab.odin.Mention

class TestCagP0 extends AgroTest {
  val mentions = extractMentions(p0s1)

  "p0s1" should "have the correct triples" in {
    // Should this just be "rainfall"?
    val rainfallNode = newNodeSpec("in rainfall", newDecrease("decrease"))
    val povertyNode = newNodeSpec("poverty", newIncrease("increased", "significantly"))
    
    val rainfallPovertyEdge = newEdgeSpec(rainfallNode, povertyNode)
    
    rainfallPovertyEdge.test(mentions) should be (true)    
  }
}
