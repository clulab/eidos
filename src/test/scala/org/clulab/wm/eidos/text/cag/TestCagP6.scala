package org.clulab.wm

import CAG._
import TestUtils._

class TestCagP6 extends Test {
  
  { // S1
    val tester = new Tester(p6s1)

    val insecurity    = NodeSpec("insecurity", Quant("Persistent"))
    val conflict      = NodeSpec("armed conflict")
    val functionality = NodeSpec("market functionality")
    val activities    = NodeSpec("livelihood activities", Dec("disrupted"))
    // NOTE: changing 'limited' to 'shrunk' (see CAG.scala) necessitates adding the Dec to access
    val access        = NodeSpec("physical access to markets", Dec("shrunk"))
  
    behavior of "p6s1"
    
    passingTest should "have correct edges 1" taggedAs(Becky) in {
      tester.test(EdgeSpec(insecurity, Causal, functionality)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Becky) in {
      tester.test(EdgeSpec(insecurity, Causal, activities)) should be (successful)
    }
    passingTest should "have correct edges 3" taggedAs(Becky) in {
      tester.test(EdgeSpec(insecurity, Causal, access)) should be (successful)
    }
    
    passingTest should "have correct edges 4" taggedAs(Becky) in {
      tester.test(EdgeSpec(conflict, Causal, functionality)) should be (successful)
    }
    passingTest should "have correct edges 5" taggedAs(Becky) in {
      tester.test(EdgeSpec(conflict, Causal, activities)) should be (successful)
    }
    passingTest should "have correct edges 6" taggedAs(Becky) in {
      tester.test(EdgeSpec(conflict, Causal, access)) should be (successful)
    }
  }
  
  { // S2
    val tester = new Tester(p6s2)

   val insecurity   = NodeSpec("levels of food insecurity", Inc("high"), Quant("high", "unprecedented"))
    // until we modify the system to (a) not suck up adjectives in the NPs and (b) store adjs as mods on NPs, the test
    // should be modified to the following:
    val fighting     = NodeSpec("widespread fighting")
    //val fighting     = newNodeSpec("fighting", newUnmarked("widespread"))
    val access       = NodeSpec("access to services", Dec("poor"), Quant("poor")) // yes
    val morbidity    = NodeSpec("morbidity", Inc("high"), Quant("high"))
    val diet         = NodeSpec("diet", Dec("poor", "extremely"), Quant("poor", "extremely"))
    val coverage     = NodeSpec("coverage of sanitation facilities", Dec("low"), Quant("low"))
    val practices    = NodeSpec("hygiene practices", Dec("poor"), Quant("poor"))
    val malnutrition = NodeSpec("Acute malnutrition", Dec("worsened")) //yes

    val displacement = NodeSpec("displacement")
  
    behavior of "p6s2"
    
    passingTest should "have correct edges 1" taggedAs(Becky) in { // good
      tester.test(EdgeSpec(insecurity, Causal, malnutrition)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Becky) in { // good
      tester.test(EdgeSpec(fighting, Causal, malnutrition)) should be (successful)
    }
    // There's no way to get this with the parse, and I would argue that this should really be a hyper edge ->
    // i.e., that the Event of Cause(displacement, access) is the cause.  While we will eventually implement this,
    // currently our causal rules operate over entities only.
    futureWorkTest should "have correct edges 3" taggedAs(Becky) in {
      tester.test(EdgeSpec(access, Causal, malnutrition)) should be (successful)
    }
    passingTest should "have correct edges 4" taggedAs(Becky) in { // good
      tester.test(EdgeSpec(morbidity, Causal, malnutrition)) should be (successful)
    }
    passingTest should "have correct edges 5" taggedAs(Becky) in { // good
      tester.test(EdgeSpec(diet, Causal, malnutrition)) should be (successful)
    }
    passingTest should "have correct edges 6" taggedAs(Becky) in {
      tester.test(EdgeSpec(coverage, Causal, malnutrition)) should be (successful)
    }
    passingTest should "have correct edges 7" taggedAs(Becky) in {
      tester.test(EdgeSpec(practices, Causal, malnutrition)) should be (successful)
    }
    passingTest should "have correct edges 8" taggedAs(Ben) in {
      tester.test(EdgeSpec(displacement, Causal, access)) should be (successful)
    }
  }

  { // S3
    val tester = new Tester(p6s3)

    val consumption  = NodeSpec("consumption of household production")
    val malnutrition = NodeSpec("levels of acute malnutrition", Inc("improvements", "marginal"))
     
    behavior of "p6s3"

    passingTest should "have correct edges 1" taggedAs(Zheng) in {
      tester.test(EdgeSpec(consumption, Causal, malnutrition)) should be (successful)
    }
  }
}
