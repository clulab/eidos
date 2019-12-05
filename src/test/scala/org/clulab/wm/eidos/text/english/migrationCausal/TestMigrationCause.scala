package org.clulab.wm.eidos.text.english.migrationCausal

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.TestUtils._

class TestMigrationCause extends EnglishTest {

  {
    val text = """ Food security was mentioned as the main reason for flight. """
    val food = NodeSpec("Food security")
    val flight = NodeSpec("flight")

    behavior of "Example 1"

    passingTest should "have correct edges 1" taggedAs(Somebody) in {
      val tester = new GraphTester(text)
      tester.test(EdgeSpec(food, Causal, flight)) should be (successful)
    }
  }

  {
    val text = """ The primary reasons for moving were insecurity, lack of food, and poor access to services such as healthcare and education. """
    val insecurity = NodeSpec("insecurity")
    val food = NodeSpec("lack of food", Dec("lack"))
    val services = NodeSpec("poor access to services", Dec("poor"), Quant("poor"))
    val moving = NodeSpec("moving")

    behavior of "Example 2"

    passingTest should "have correct edges 1" taggedAs(Somebody) in {
      val tester = new GraphTester(text)
      tester.test(EdgeSpec(insecurity, Causal, moving)) should be (successful)
      tester.test(EdgeSpec(food, Causal, moving)) should be (successful)
      tester.test(EdgeSpec(services, Causal, moving)) should be (successful)
    }
  }

  {
    val text = """ The gap is mainly attributable to lack of adequate local construction materials. """
    val gap = NodeSpec("gap")
    val materials = NodeSpec("lack of adequate local construction materials", Dec("lack"), Quant("adequate"))

    behavior of "Example 3"

    passingTest should "have correct edges 1" taggedAs(Somebody) in {
      val tester = new GraphTester(text)
      tester.test(EdgeSpec(materials, Causal, gap)) should be (successful)
    }
  }

  {
    val text = """ Refugees continue to report the fear of indiscriminate killings by the government forces as their main reason of flight. """
    val fear = NodeSpec("fear of indiscriminate killings by the government forces")
    val flight = NodeSpec("main reason of flight")

    behavior of "Example 4"

    passingTest should "have correct edges 1" taggedAs(Somebody) in {
      val tester = new GraphTester(text)
      tester.test(EdgeSpec(fear, Causal, flight)) should be (successful)
    }
  }
}

