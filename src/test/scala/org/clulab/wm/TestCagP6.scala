package org.clulab.wm

import CAG._
import TestUtils._

class TestCagP6 extends Test {
  
  { // S1
    val tester = new Tester(p6s1)

    val insecurity    = newNodeSpec("insecurity", newQuantification("persistent"))
    val conflict      = newNodeSpec("armed conflict")
    val functionality = newNodeSpec("market functionality")
    val activities    = newNodeSpec("livelihood activities")
    val access        = newNodeSpec("physical access to markets")
  
    "p6s1" should "not be ignored" in {
    }
    ignore should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(insecurity, Affect, functionality)) should be (successful)
    }
    ignore should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(insecurity, Causal, activities)) should be (successful)
    }
    ignore should "have correct edges 3" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(insecurity, Causal, access)) should be (successful)
    }
    
    ignore should "have correct edges 4" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(conflict, Affect, functionality)) should be (successful)
    }
    ignore should "have correct edges 5" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(conflict, Causal, activities)) should be (successful)
    }
    ignore should "have correct edges 6" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(conflict, Causal, access)) should be (successful)
    }
  }
  
  { // S2
    val tester = new Tester(p6s2)

    val insecurity   = newNodeSpec("food insecurity", newIncrease("high", "unprecedented"))
    val fighting     = newNodeSpec("fighting", newUnmarked("widespread"))
    val access       = newNodeSpec("access to services", newDecrease("poor"))
    val morbidity    = newNodeSpec("morbidity", newIncrease("high"))
    val diet         = newNodeSpec("diet", newDecrease("poor", "extremely"))
    val coverage     = newNodeSpec("coverage of sanitation facilities", newDecrease("low"))
    val practices    = newNodeSpec("hygiene practices", newDecrease("poor"))
    val malnutrition = newNodeSpec("acute malnutrition", newDecrease("worsened"))
    
    val displacement = newNodeSpec("displacement")
  
    "p6s2" should "not be ignored" in {
    }
    ignore should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(insecurity, Causal, malnutrition)) should be (successful)
    }
    ignore should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(fighting, Causal, malnutrition)) should be (successful)
    }
    ignore should "have correct edges 3" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(access, Causal, malnutrition)) should be (successful)
    }
    
    ignore should "have correct edges 4" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(morbidity, Causal, malnutrition)) should be (successful)
    }
    ignore should "have correct edges 5" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(diet, Causal, malnutrition)) should be (successful)
    }
    ignore should "have correct edges 6" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(coverage, Causal, malnutrition)) should be (successful)
    }
    ignore should "have correct edges 7" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(practices, Causal, malnutrition)) should be (successful)
    }

    ignore should "have correct edges 8" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(displacement, Causal, access)) should be (successful)
    }
  }

  { // S3
    val tester = new Tester(p6s3)

    val consumption  = newNodeSpec("consumption of household production")
    val malnutrition = newNodeSpec("acute malnutrition", newIncrease("worsened", "marginal"))
     
    "p6s3" should "not be ignored" in {
    }
    ignore should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(newEdgeSpec(consumption, Causal, malnutrition)) should be (successful)
    }
  }
}
