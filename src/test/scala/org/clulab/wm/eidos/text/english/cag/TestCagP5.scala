package org.clulab.wm.eidos.text.english.cag

import CAG._

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.TestUtils._

class TestCagP5 extends EnglishTest {
  
  { // S1
    val tester = new GraphTester(p5s1)
  
    behavior of "p5s1"
  }
  
  { // S2
    val tester = new GraphTester(p5s2)

    val attacks = NodeSpec("repeated attacks")
    val many    = NodeSpec("Many", Unmarked("displaced"))
  
    behavior of "p5s2"
    
    futureWorkTest should "have correct edges 1" taggedAs(Becky) in {
      tester.test(EdgeSpec(attacks, Causal, many)) should be (successful)
    }
  }
}
