package org.clulab.wm.eidos.text.portuguese

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.PortugueseTest
import org.clulab.wm.eidos.test.TestUtils._

class TestPortugueseDoc1 extends PortugueseTest {
  // Text extracted from document S1415-43662014001200005 on Scielo
  { // Paragraph 1
    val text = """Resultados recentes de pesquisas apontam que condições adequadas de umidade
              favorecem a absorção de nutrientes, em especial daqueles cujo mecanismo principal de transporte no solo é a difusão,
               como é o caso do fósforo (Costa et al., 2006; Oliveira et al., 2010)."""

    val umidade = NodeSpec("condições adequadas de umidade")
    val nutrientes = NodeSpec("a absorção de nutrientes", Inc("favorecem"))

    behavior of "TestPortugueseDoc1 Paragraph 1"

    waitingForProcessors should "have correct edges 1" taggedAs(George) in {
      val tester = new GraphTester(text)
      tester.test(EdgeSpec(umidade, Causal, nutrientes)) should be (successful)
    }
  }
  { // Paragraph 2
    val text = """Assim, o uso da irrigação e da fertirrigação é uma das formas de maximizar a disponibilidade de fósforo no solo para que seja aproveitado de forma eficiente pelo cafeeiro."""

    val irrigacao = NodeSpec("o uso da irrigação")
    val fosforo = NodeSpec("a disponibilidade de fósforo", Inc("maximizar"))

    behavior of "TestPortugueseDoc1 Paragraph 2"
    /* TODO: correct acl rule
    passingTest should "have correct edges 1" taggedAs(George) in {
      val tester = new Tester(text, "portuguese")
      tester.test(EdgeSpec(irrigacao, Causal, fosforo)) should be (successful)
    }*/
  }
  { // Paragraph 3
    val text = """Segundo Zanini o aumento na distribuição de fósforo no solo utilizando-se fertirrigação por gotejamento, ocorre de maneira mais expressiva haja vista que airrigação provoca aumento na umidade em uma faixa estreita saturando os sítios de fixação próximos ao ponto de aplicação."""

    val irrigacao = NodeSpec("que airrigação")
    val umidade = NodeSpec("aumento na umidade em uma faixa estreita", Inc("aumento"))


    behavior of "TestPortugueseDoc0 Paragraph 3"

    waitingForProcessors should "have correct edges 1" taggedAs(George) in {
      val tester = new GraphTester(text)
      tester.test(EdgeSpec(irrigacao, Causal, umidade)) should be (successful)
    }
  }
  { // Paragraph 4
    val text = """Maiores lâminas de irrigação aplicadas no cafeeiro aumentam o teor foliar desse nutriente."""

    val irrigacao = NodeSpec("Maiores lâminas de irrigação aplicadas no cafeeiro")
    val nutriente = NodeSpec("o teor foliar desse nutriente", Inc("aumentam"))

    behavior of "TestPortugueseDoc0 Paragraph 4"

    waitingForProcessors should "have correct edges 1" taggedAs(George) in {
      val tester = new GraphTester(text)
      tester.test(EdgeSpec(irrigacao, Causal, nutriente)) should be (successful)
    }
  }
}
