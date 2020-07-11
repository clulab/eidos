package org.clulab.wm.eidos.text.english.cag

import CAG._

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.TestUtils._

class TestCagP4 extends EnglishTest {
  
  { // S1
    val tester = new GraphTester(p4s1)
  
    val cost = NodeSpec("rising cost of living", Inc("rising"))
    val impact = NodeSpec("impact of the conflict")
    val ability = NodeSpec("people's ability to access safe water", Dec("undermined"))
    
    behavior of "p4s1"

    passingTest should "have correct edges 1" taggedAs(Mithun) in {
      tester.test(EdgeSpec(cost, Causal, ability)) should be (successful)
    }
    // This test relies upon traversing several nmods.  Rules which capture it seem to overmatch, but this can be
    // worked on for sure...
    futureWorkTest should "have correct edges 2" taggedAs(Mithun) in {
      tester.test(EdgeSpec(impact, Causal, ability)) should be (successful)
    }
  }
  
  { // S2
    val tester = new GraphTester(p4s2)
  
    behavior of "p4s2"
  }
  
  { // S3
    val tester = new GraphTester(p4s3)
    // water trucking has decreased due to the cost of fuel
    val cost     = NodeSpec("cost of fuel")
    val trucking = NodeSpec("water trucking", Dec("decreased"))
  
    behavior of "p4s3"
    
    passingTest should "have correct edges 1" taggedAs(Ben) in {
      tester.test(EdgeSpec(cost, Causal, trucking)) should be (successful)
    }
  }
  
  { // S4
    val tester = new GraphTester(p4s4)

    val insecurity = NodeSpec("insecurity")
    val expertise  = NodeSpec("lack of technical expertise and supplies", Dec("lack"))
//    val supplies   = NodeSpec("supplies", Dec("lack"))
    val access     = NodeSpec("access", Dec("lack"))
    val repairs    = NodeSpec("Borehole repairs", Dec("possible"))

    //TO-DO:  "have not been possible" is not be recognized as "inhibit", it is not currently supported yet
    //val repairs    = NodeSpec("Borehole repairs", Dec("inhibit"))
  
    behavior of "p4s4"

    passingTest should "have correct edges 1" taggedAs(Fan) in {
      tester.test(EdgeSpec(insecurity, Causal, access)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Fan) in {
      tester.test(EdgeSpec(expertise, Causal, repairs)) should be (successful)
    }
    // conjunctions and non-contiguous entities
//    tempBrokenEntitiesTest should "have correct edges 3" taggedAs(Fan) in {
//      tester.test(EdgeSpec(supplies, Causal, repairs)) should be (successful)
//    }
    passingTest should "have correct edges 4" taggedAs(Fan) in {
      tester.test(EdgeSpec(access, Causal, repairs)) should be (successful)
    }
  }
}

