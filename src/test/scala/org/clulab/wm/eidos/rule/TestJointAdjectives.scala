package org.clulab.wm.eidos.rule

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.{AntiNodeSpec, NodeSpec, AntiEdgeSpec, EdgeSpec}
import org.clulab.wm.eidos.text.Causal

class TestJointAdjectives extends Test {

  {
    val text = "Yellow and blue states cause contention."

    val tester = new Tester(text)

    val antiYellow = AntiNodeSpec("Yellow")
    val antiYellowStates = AntiNodeSpec("Yello states")
    val antiBlue = AntiNodeSpec("blue")
    val blueStates = NodeSpec("blue states")
    val antiBlueStates = NodeSpec("blue states")
    val yellowAndBlueStates = NodeSpec("Yellow and blue states")
    val contention = NodeSpec("contention")

    behavior of "Joint adjectives"

    it should "not get the first one alone" taggedAs(Somebody) in {
      tester.test(antiYellow) should be (successful)
    }
    it should "not get the second one alone" taggedAs(Somebody) in {
      tester.test(antiBlue) should be (successful)
    }
    it should "not get the first noun phrase" taggedAs(Somebody) in {
      tester.test(antiYellowStates) should be (successful)
    }
    it should "find the effect" taggedAs(Somebody) in {
      tester.test(contention) should be (successful)
    }
    it should "not get the second noun phrase (in an event)" taggedAs(Somebody) in {
      //tester.test(blueStates) should be (successful) // This might be OK, but
      tester.test(AntiEdgeSpec(blueStates, Causal, contention)) // this isn't.
    }
    it should "get the combined noun phrase" taggedAs(Somebody) in {
      tester.test(yellowAndBlueStates) should be (successful)
    }
    it should("also find the edge") taggedAs(Somebody) in {
      tester.test(EdgeSpec(yellowAndBlueStates, Causal, contention)) should be (successful)
    }
  }

  {
    val text = "Red states and blue states cause confusion."

    val tester = new Tester(text)

    val redStates = NodeSpec("Red states")
    val blueStates = NodeSpec("blue states")
    val antiRedStatesAndBlueStates = AntiNodeSpec("Red states and blue states")
    val confusion = NodeSpec("confusion")

    behavior of "Joint noun phrases"

    it should "get the first one" taggedAs(Somebody) in {
      tester.test(redStates) should be (successful)
    }
    it should "get the second one" taggedAs(Somebody) in {
      tester.test(blueStates) should be (successful)
    }
    it should "not get the two combined" taggedAs(Somebody) in {
      tester.test(antiRedStatesAndBlueStates) should be (successful)
    }
    it should "find the effect" taggedAs(Somebody) in {
      tester.test(confusion) should be (successful)
    }
    it should "get both edges" taggedAs(Somebody) in {
      tester.test(EdgeSpec(redStates, Causal, confusion)) should be (successful)
      tester.test(EdgeSpec(blueStates, Causal, confusion)) should be (successful)
    }
  }

  {
    // This documents current functionality.  If these tests fail, then
    // processors may have been improved and we'd like to know that.

    val text = "Red and blue states cause problems." // Red is NNP

    val tester = new Tester(text)

    val red = NodeSpec("Red")
    val antiRedStates = AntiNodeSpec("Red states")
    val blueStates = NodeSpec("blue states")
    val antiRedAndBlueStates = AntiNodeSpec("Red and blue states")
    val problems = NodeSpec("problems")

    behavior of "Misparsed adjectives in noun phrases"

    it should "get misparsed one" taggedAs(Somebody) in {
      tester.test(red) should be (successful)
    }
    it should "not combine it with the noun" taggedAs(Somebody) in {
      tester.test(antiRedStates) should be (successful)
    }
    it should "get the second one" taggedAs(Somebody) in {
      tester.test(blueStates) should be (successful)
    }
    it should "not combine them" taggedAs(Somebody) in {
      tester.test(antiRedAndBlueStates) should be (successful)
    }
    it should "find the two events" taggedAs(Somebody) in {
      tester.test(EdgeSpec(red, Causal, problems)) should be (successful)
      tester.test(EdgeSpec(blueStates, Causal, problems)) should be (successful)
    }
  }
}
