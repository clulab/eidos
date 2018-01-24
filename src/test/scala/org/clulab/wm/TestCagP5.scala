package org.clulab.wm

import CAG._
import TestUtils._

class TestCagP5 extends Test {
  
  { // S1
    val tester = new Tester(p5s1)
  
    "p5s1" should "not be ignored" in {
    }
  }
  
  { // S2
    val tester = new Tester(p5s2)

    val attacks = newNodeSpec("attacks", newUnmarked("repeated"))
    val many    = newNodeSpec("many", newUnmarked("displaced"))
  
    "p5s2" should "not be ignored" in {
    }
    ignore should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(attacks, Causal, many)) should be (successful)
    }
  }
}
