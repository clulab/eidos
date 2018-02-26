package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.{Inc, Dec, Quant, Causal, Origin, EdgeSpec, NodeSpec}


class TestRaps extends Test {


  { //One Increase Event
    val sent1 = "Better and well-functioning agricultural credit and market services for both established and emerging farmers."
    val tester = new Tester(sent1)

    val credit = NodeSpec("well-functioning agricultural credit", Inc("Better") )

    behavior of "Raps_sent1"

    passingTest should "have the correct node" taggedAs(Heather) in {
      tester.test(credit) should be (successful)

    }

  }

  { //3 Increase events
    val sent2 = "The support for agricultural research, education, and extension programs will also be increased for developing and disseminating climate change adaptation agricultural technologies to the farmers."
    // Note: parse of sentence makes it "impossible" to extract increase for education and extension programs
    // Maybe a reason to switch to cluprocessor
    val tester = new Tester(sent2)

    //ALL of these fail to be increase events
    val research = NodeSpec("agricultural research", Inc("increased"))
    val education = NodeSpec("education", Inc("increased"))
    val programs = NodeSpec("extension programs", Inc("increased"))

    behavior of "Raps_sent2"

    failingTest should "have correct nodes 1" taggedAs(Heather) in {
      tester.test(research) should be (successful)
    }

    failingTest should "have correct nodes 2" taggedAs(Heather) in {
      tester.test(education) should be (successful)
    }

    failingTest should "have correct nodes 3" taggedAs(Heather) in {
      tester.test(programs) should be (successful)
    }

  }

  { //3 Decrease, 2 Increase Events; 2 Causal Edges, 1 Origin Edge
    val sent3 = "Limited financial capacities and low education levels further restrict farmers' ability for higher benefits from increased agricultural production."
    val tester = new Tester(sent3)

    val financial = NodeSpec("Limited financial capacities", Dec("Limited"))
    val ability = NodeSpec("\' ability", Dec("restrict"))
    val education = NodeSpec("education levels", Dec("low"), Quant("low"))

    val production = NodeSpec("agricultural production", Inc("increased"))
    val benefits = NodeSpec("benefits", Inc("higher"))

    behavior of "Raps_sent3"

    passingTest should "have correct edges 1" taggedAs(Heather) in {
      tester.test(EdgeSpec(financial, Causal, ability)) should be (successful)
    }

    passingTest should "have correct edges 2" taggedAs(Heather) in {
      tester.test(EdgeSpec(education, Causal, ability)) should be (successful)
    }

    //The issue is that Origin is an instance of EventSpec, not EdgeSpec,
    //but EventSpec seemily cannot be use as an object here.
    futureWorkTest should "have correct edges 3" taggedAs(Heather) in {
      tester.test(EdgeSpec(production, Origin, benefits)) should be (successful)
    }

  }

  
} //END OF TEST BRACE


