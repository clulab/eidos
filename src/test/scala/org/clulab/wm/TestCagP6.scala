package org.clulab.wm

import CAG._
import TestUtils._

class TestCagP6 extends Test {
  
  { // S1
    val tester = new Tester(p6s1)

    val insecurity    = newNodeSpec("insecurity", newQuantification("Persistent"))
    val conflict      = newNodeSpec("armed conflict")
    val functionality = newNodeSpec("market functionality")
    val activities    = newNodeSpec("livelihood activities", newDecrease("disrupted"))
    // NOTE: changing 'limited' to 'shrunk' (see CAG.scala) necessitates adding the newDecrease to access
    val access        = newNodeSpec("physical access to markets", newDecrease("shrunk"))
  
    behavior of "p6s1"
    
    passingTest should "have correct edges 1" taggedAs(Becky) in {
      tester.test(newEdgeSpec(insecurity, Affect, functionality)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Becky) in {
      tester.test(newEdgeSpec(insecurity, Causal, activities)) should be (successful)
    }
    passingTest should "have correct edges 3" taggedAs(Becky) in {
      tester.test(newEdgeSpec(insecurity, Causal, access)) should be (successful)
    }
    
    passingTest should "have correct edges 4" taggedAs(Becky) in {
      tester.test(newEdgeSpec(conflict, Affect, functionality)) should be (successful)
    }
    passingTest should "have correct edges 5" taggedAs(Becky) in {
      tester.test(newEdgeSpec(conflict, Causal, activities)) should be (successful)
    }
    passingTest should "have correct edges 6" taggedAs(Becky) in {
      tester.test(newEdgeSpec(conflict, Causal, access)) should be (successful)
    }
  }
  
  { // S2
    val tester = new Tester(p6s2)

    val insecurity   = newNodeSpec("levels of food insecurity", newIncrease("high"), newQuantification("high", "unprecedented"))
    // until we modify the system to (a) not suck up adjectives in the NPs and (b) store adjs as mods on NPs, the test
    // should be modified to the following:
    val fighting     = newNodeSpec("widespread fighting")
    //val fighting     = newNodeSpec("fighting", newUnmarked("widespread"))
    val access       = newNodeSpec("access to services", newDecrease("poor"), newQuantification("poor")) // yes
    val morbidity    = newNodeSpec("morbidity", newIncrease("high"), newQuantification("high"))
    val diet         = newNodeSpec("diet", newDecrease("poor", "extremely"), newQuantification("poor", "extremely"))
    val coverage     = newNodeSpec("coverage of sanitation facilities", newDecrease("low"), newQuantification("low"))
    val practices    = newNodeSpec("hygiene practices", newDecrease("poor"), newQuantification("poor"))
    val malnutrition = newNodeSpec("Acute malnutrition", newDecrease("worsened")) //yes
    
    val displacement = newNodeSpec("displacement")
  
    behavior of "p6s2"
    
    passingTest should "have correct edges 1" taggedAs(Becky) in { // good
      tester.test(newEdgeSpec(insecurity, Causal, malnutrition)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Becky) in { // good
      tester.test(newEdgeSpec(fighting, Causal, malnutrition)) should be (successful)
    }
    // There's no way to get this with the parse, and I would argue that this should really be a hyper edge ->
    // i.e., that the Event of Cause(displacement, access) is the cause.  While we will eventually implement this,
    // currently our causal rules operate over entities only.
    futureWorkTest should "have correct edges 3" taggedAs(Becky) in {
      tester.test(newEdgeSpec(access, Causal, malnutrition)) should be (successful)
    }
    passingTest should "have correct edges 4" taggedAs(Becky) in { // good
      tester.test(newEdgeSpec(morbidity, Causal, malnutrition)) should be (successful)
    }
    passingTest should "have correct edges 5" taggedAs(Becky) in { // good
      tester.test(newEdgeSpec(diet, Causal, malnutrition)) should be (successful)
    }
    passingTest should "have correct edges 6" taggedAs(Becky) in {
      tester.test(newEdgeSpec(coverage, Causal, malnutrition)) should be (successful)
    }
    passingTest should "have correct edges 7" taggedAs(Becky) in {
      tester.test(newEdgeSpec(practices, Causal, malnutrition)) should be (successful)
    }
    passingTest should "have correct edges 8" taggedAs(Ben) in {
      tester.test(newEdgeSpec(displacement, Causal, access)) should be (successful)
    }
  }

  { // S3
    val tester = new Tester(p6s3)

    val consumption  = newNodeSpec("consumption of household production")
    val malnutrition = newNodeSpec("levels of acute malnutrition", newIncrease("improvements", "marginal"))
     
    behavior of "p6s3"

    passingTest should "have correct edges 1" taggedAs(Zheng) in {
      tester.test(newEdgeSpec(consumption, Causal, malnutrition)) should be (successful)
    }
  }
}
