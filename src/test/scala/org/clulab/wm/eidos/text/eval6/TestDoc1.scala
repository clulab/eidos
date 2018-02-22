package org.clulab.wm.eidos.text.eval6

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.Causal
import org.clulab.wm.eidos.text.Dec
import org.clulab.wm.eidos.text.EdgeSpec
import org.clulab.wm.eidos.text.Inc
import org.clulab.wm.eidos.text.NodeSpec
import org.clulab.wm.eidos.text.Quant

class TestDoc1 extends Test {
  // 130035 excerpt BG.pdf
  { // Paragraph 1
    val text = """
      Copy the text here
      """

    val concerns = NodeSpec("Concerns about insufficient food access") // Make some node spec
    
    behavior of "TestDoc1 Paragraph 1"

    failingTest should "have correct edges 1" taggedAs(Becky) in {
      val tester = new Tester(text)
  
      tester.test(EdgeSpec(concerns, Causal, concerns)) should be (successful) // Test edges connecting them
    }
  }
}
