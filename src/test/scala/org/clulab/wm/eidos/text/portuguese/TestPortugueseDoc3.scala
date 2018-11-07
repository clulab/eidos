package org.clulab.wm.eidos.text.portuguese

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.TestUtils._

class TestPortugueseDoc3 extends PortugueseTest {
  //
  { // Paragraph 1
    val text = """O sêmen ativado por NaHCO3 60 e 119 mM apresentou as maiores taxas de motilidade espermática."""

    val aplicacao = NodeSpec("A aplicação de Si")
    val plantas = NodeSpec("aumento do crescimento das plantas cultivadas em vasos")

    behavior of "TestPortugueseDoc2 Paragraph 1"

    waitingForProcessors should "have correct edges 1" taggedAs(George) in {
      val tester = new GraphTester(text)
      tester.test(EdgeSpec(aplicacao, Causal, plantas)) should be (successful)
    }
  }

}
