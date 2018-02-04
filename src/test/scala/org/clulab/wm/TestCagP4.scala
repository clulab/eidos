package org.clulab.wm

import CAG._
import TestUtils._

class TestCagP4 extends Test {
  
  { // S1
    val tester = new Tester(p4s1)
  
    val cost = newNodeSpec("cost of living", newIncrease("rising"))
    val impact = newNodeSpec("impact of the conflict")
    val ability = newNodeSpec("people's ability to access safe water", newDecrease("undermined"))
    
    behavior of "p4s1"
    
    it should "have correct edges 1" taggedAs(Mithun) in {
      tester.test(newEdgeSpec(cost, Causal, ability)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Mithun) in {
      tester.test(newEdgeSpec(impact, Causal, ability)) should be (successful)
    }
  }
  
  { // S2
    val tester = new Tester(p4s2)
  
    behavior of "p4s2"
  }
  
  { // S3
    val tester = new Tester(p4s3)
    // water trucking has decreased due to the cost of fuel
    val cost     = newNodeSpec("cost of fuel")
    val trucking = newNodeSpec("water trucking", newDecrease("decreased"))
  
    behavior of "p4s3"
    
    passingTest should "have correct edges 1" taggedAs(Ben) in {
      tester.test(newEdgeSpec(cost, Causal, trucking)) should be (successful)
    }
  }
  
  { // S4
    val tester = new Tester(p4s4)

    val insecurity = newNodeSpec("insecurity")
    val expertise  = newNodeSpec("technical expertise", newDecrease("lack"))
    val supplies   = newNodeSpec("supplies", newDecrease("lack"))
    val access     = newNodeSpec("access", newDecrease("lack"))
    val repairs    = newNodeSpec("Borehole repairs", newDecrease("inhibit"))
  
    behavior of "p4s4"
    
    failingTest should "have correct edges 1" taggedAs(Fan) in {
      tester.test(newEdgeSpec(insecurity, Causal, access)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Fan) in {
      tester.test(newEdgeSpec(expertise, Causal, access)) should be (successful)
    }
    failingTest should "have correct edges 3" taggedAs(Fan) in {
      tester.test(newEdgeSpec(supplies, Causal, access)) should be (successful)
    }
    failingTest should "have correct edges 4" taggedAs(Fan) in {
      tester.test(newEdgeSpec(access, Causal, repairs)) should be (successful)
    }
  }
}

