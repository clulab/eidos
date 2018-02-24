package org.clulab.wm.eidos.text.cag

import CAG._

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.Unmarked
import org.clulab.wm.eidos.text.NodeSpec
import org.clulab.wm.eidos.text.EdgeSpec
import org.clulab.wm.eidos.text.Causal
import org.scalactic.source.Position.apply

class TestCagP5 extends Test {
  
  { // S1
    val tester = new Tester(p5s1)
  
    behavior of "p5s1"
  }
  
  { // S2
    val tester = new Tester(p5s2)

    val attacks = NodeSpec("repeated attacks")
    val many    = NodeSpec("Many", Unmarked("displaced"))
  
    behavior of "p5s2"
    
    futureWorkTest should "have correct edges 1" taggedAs(Becky) in {
      tester.test(EdgeSpec(attacks, Causal, many)) should be (successful)
    }
  }
}
