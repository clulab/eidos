package org.clulab.wm.eidos.text.english.cag

import CAG._

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.TestUtils._

class TestCagP3 extends EnglishTest {
  
  { // S1
    val tester = new GraphTester(p3s1)

    val foodInsecurity = NodeSpec("Food insecurity", Inc("deepening"), Quant("severe")) // todo: add ability to have 'more'?
    val conflict       = NodeSpec("conflict")
    val displacement   = NodeSpec("displacement")
    val people         = NodeSpec("people", Quant("vulnerable"))
    
    behavior of "p3s1"

    passingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(conflict, Causal, people)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Ajay) in {
      tester.test(EdgeSpec(displacement, Causal, people)) should be (successful)
    }
    passingTest should "have correct singleton node 1" taggedAs(Ajay) in {
      tester.test(foodInsecurity) should be (successful)
    }
  }
  
  { // S2
    val tester = new GraphTester(p3s2)
   // tester.mentions.foreach(m => println("\t" + m.text))

    val impacts = NodeSpec("impacts of flooding")
    val economic = NodeSpec("economic collapse", Dec("collapse"))
    val conflict = NodeSpec("conflict")
    val production = NodeSpec("agricultural production", Dec("reduced"))
    val insecurity = NodeSpec("2017 , food insecurity in Unity , Jonglei and parts of Greater Equatoria and Greater Bahr el Ghazal remained critical", Quant("critical"), TimEx("2017"))
    
    behavior of "p3s2"

    passingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(impacts, Causal, production)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(economic, Causal, production)) should be (successful)
    }
    // The parse for this sentence is too bad to get this, but as I keep trying to modify the sentence to fix it,
    // the parse breaks in other ways... hopefully we can fix this with CluProcessor
    brokenSyntaxTest should "have correct edges 3" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(conflict, Causal, production)) should be (successful)
    }
    passingTest should "have correct edges 4" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(production, Correlation, insecurity)) should be (successful)
    }
  }
  
  { // S3
    val tester = new GraphTester(p3s3)
  
    behavior of "p3s3"
  }
  
  { // S4
    val tester = new GraphTester(p3s4)

    val harvest = NodeSpec("harvest")
    val prices  = NodeSpec("food prices", Inc("high"))
  
    behavior of "p3s4"

    affectEventTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(harvest, Affect, prices)) should be (successful)
    }
  }
  
  { // S5
    val tester = new GraphTester(p3s5)

    val economic    = NodeSpec("economic decline", Dec("decline"))
    val accessFood  = NodeSpec("access to staple food", Dec("reduction"))
    // becky: with current tokenInterval restrictions, can't have access to clean water (i.e., split interval)
    val accessWater = NodeSpec("clean water", Dec("reduction"))
    val foods       = NodeSpec("variety of foods", Dec("reduction"))
  
    behavior of "p3s5"
    //waiting on "economic" fix from becky. 

    tempBrokenEntitiesTest should "have correct edges 1" taggedAs(Mithun) in {
      tester.test(EdgeSpec(economic, Causal, accessFood)) should be (successful)
    }
    tempBrokenEntitiesTest should "have correct edges 2" taggedAs(Mithun) in {
      tester.test(EdgeSpec(economic, Causal, accessWater)) should be (successful)

    }
    // Becky: sentence modified slightly to provide a more reasonable parse.
    brokenSyntaxTest should "have correct edges 3" taggedAs(Mithun) in {
      tester.test(EdgeSpec(economic, Causal, foods)) should be (successful)
    }
  }  
}
