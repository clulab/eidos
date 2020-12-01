package org.clulab.wm.eidos.text.english.eval6

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.EnglishTest
import org.clulab.wm.eidos.test.TestUtils._

class TestDoc0 extends EnglishTest {
  
  { // Paragraph 1
    val text = """An adequate supply of food at the national or international level does not in itself guarantee
household level food security. Concerns about insufficient food access have resulted in a greater
policy focus on incomes, expenditure, markets and prices in achieving food security objectives.
      """
    val concerns = NodeSpec("Concerns about insufficient food access")
    val focus = NodeSpec("policy focus on incomes", Inc("greater"))
    val expenditure = NodeSpec("expenditure")
    val markets = NodeSpec("markets")
    val prices = NodeSpec("prices in achieving food security objectives")
    val policy = NodeSpec("policy")
    
    behavior of "TestDoc0 Paragraph 1"

    tempBrokenEntitiesTest should "have correct edges 1" taggedAs(Somebody) in {
      // Placing the tester here will help it be ignored
      val tester = new GraphTester(text)

      tester.test(EdgeSpec(concerns, Causal, focus)) should be (successful)
    }
  }
}
