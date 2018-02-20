package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.NodeSpec
import org.clulab.wm.eidos.text.Inc
import org.scalactic.source.Position.apply

class TestRaps extends Test {


  { //One Increase Event
    val sent1 = "Better and well-functioning agricultural credit and market services for both established and emerging farmers."
    val tester = new Tester(sent1)

    val credit = NodeSpec("well-functioning agricultural credit", Inc("Better") )

    behavior of "Raps_sent1"

    passingTest should "have the correct edge" taggedAs(Heather) in {

      tester.test(credit) should be (successful)

    }

  }

  
} //END OF TEST BRACE


