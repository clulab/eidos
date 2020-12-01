package org.clulab.wm.eidos.text.portuguese

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.PortugueseTest
import org.clulab.wm.eidos.test.TestUtils._

class TestPortugueseDoc0 extends PortugueseTest {
  // Text extracted from document S1413-70542010000100031 on Scielo
  { // Paragraph 1
    val text = """A suplementação protéico-energética com 0,6% de MS em relação ao peso do animal,
               durante a época das águas, nas condições do presente trabalho,
                promoveu aumento no desempenho de bovinos mantidos em pastagem."""

    val suplementacao = NodeSpec("A suplementação protéico-energética com 0,6% de MS em relação ao peso do animal, durante a época das águas, nas condições do presente trabalho")
    val bovinos = NodeSpec("aumento no desempenho de bovinos mantidos em pastagem", Inc("promoveu"), Inc("aumento"))

    behavior of "TestPortugueseDoc0 Paragraph 1"

    waitingForProcessors should "have correct edges 1" taggedAs(George) in {
      val tester = new GraphTester(text)
      tester.test(EdgeSpec(suplementacao, Causal, bovinos)) should be (successful)
    }
  }
  { // Paragraph 2
    val text = """A suplementação de bovinos, durante o período das águas, promoveu aumento na produção de carne/ha."""

    val suplementacao = NodeSpec("A suplementação de bovinos, durante o período das águas")
    val producao = NodeSpec("aumento na produção de carne", Inc("promoveu"), Inc("aumento"))

    behavior of "TestPortugueseDoc0 Paragraph 2"

    waitingForProcessors should "have correct edges 1" taggedAs(George) in {
      val tester = new GraphTester(text)
      tester.test(EdgeSpec(suplementacao, Causal, producao)) should be (successful)
    }
  }
  { // Paragraph 2
    val text = """Sistemas de pastejo manejados e adubados adequadamente permitem altas produções de massa seca, permitindo otimizar o desempenho animal."""

    val pastejo = NodeSpec("Sistemas de pastejo manejados")
    val massa = NodeSpec("altas produções de massa seca")
    val desempenho = NodeSpec("o desempenho animal", Inc("otimizar"))

    behavior of "TestPortugueseDoc0 Paragraph 3"

    waitingForProcessors should "have correct edges 1" taggedAs(George) in {
      val tester = new GraphTester(text)
      tester.test(EdgeSpec(pastejo, Causal, massa)) should be (successful)
    }

    waitingForProcessors should "have correct edges 2" taggedAs(George) in {
      val tester = new GraphTester(text)
      tester.test(EdgeSpec(pastejo, Causal, desempenho)) should be (successful)
    }
  }
}
