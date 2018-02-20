package org.clulab.wm.eidos.text.eval6

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.Causal
import org.clulab.wm.eidos.text.Dec
import org.clulab.wm.eidos.text.EdgeSpec
import org.clulab.wm.eidos.text.Inc
import org.clulab.wm.eidos.text.NodeSpec
import org.clulab.wm.eidos.text.Quant

class TestDoc0 extends Test {
  
  { // Paragraph 1
    val text = """An adequate supply of food at the national or international level does not in itself guarantee
household level food security. Concerns about insufficient food access have resulted in a greater
policy focus on incomes, expenditure, markets and prices in achieving food security objectives.
      """
    val tester = new Tester(text)
  
    val concerns = NodeSpec("Concerns about insufficient food access")
    val focus = NodeSpec("policy focus on incomes", Inc("greater"))
    val expenditure = NodeSpec("expenditure")
    val markets = NodeSpec("markets")
    val prices = NodeSpec("prices in achieving food security objectives")
    val policy = NodeSpec("policy")
    
    behavior of "TestDoc0 Paragraph 1"

    passingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(concerns, Causal, focus)) should be (successful)
    }
  }
}
