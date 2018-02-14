package org.clulab.wm

import org.scalatest._
import TestUtils._

class TestRaps extends Test {


  { //One Increase Event
    val sent1 = "Better and well-functioning agricultural credit and market services for both established and emerging farmers."
    val tester = new Tester(sent1)

    val credit = newNodeSpec("well-functioning agricultural credit", newIncrease("Better") )

    behavior of "Raps_sent1"

    passingTest should "have the correct edge" taggedAs(Heather) in {

      tester.test(credit) should be (successful)

    }

  }
  


} //END OF TEST BRACE


