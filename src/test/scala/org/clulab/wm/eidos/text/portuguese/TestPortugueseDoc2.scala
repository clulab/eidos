package org.clulab.wm.eidos.text.portuguese

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.TestUtils._

class TestPortugueseDoc2 extends PortugueseTest {
  /*
  // Text extracted from document S1516-35982007000300002 on Scielo
  { // Paragraph 1
    val text = """O sêmen ativado por NaHCO3 60 e 119 mM apresentou as maiores taxas de motilidade espermática."""

    val semen = NodeSpec("O sêmen")
    val composto = NodeSpec("NaHCO3 60 e 119 mM")

    behavior of "TestPortugueseDoc2 Paragraph 1"

    passingTest should "have correct edges 1" taggedAs(George) in {
      val tester = new GraphTester(text)
      tester.test(EdgeSpec(composto, Causal, semen)) should be (successful)
    }
  }
  { // Paragraph 2
    val text = """A ativação por NaHCO3 119 mM possibilitou as maiores durações de motilidade no sêmen de P. lineatus."""

    val ativacao = NodeSpec("A ativação por NaHCO3 119 mM")
    val motilidade = NodeSpec("as maiores durações de motilidade no sêmen de P. lineatus")// no sêmen de P. lineatus")

    behavior of "TestPortugueseDoc2 Paragraph 2"

    passingTest should "have correct edges 1" taggedAs(George) in {
      val tester = new GraphTester(text)
      tester.test(EdgeSpec(ativacao, Causal, motilidade)) should be (successful)
    }
  }
  { // Paragraph 3
    val text = """Porém, a ativação por NaHCO3 60 mM e 119 mM propiciou as maiores taxas de motilidade espermática."""

    val ativacao = NodeSpec("a ativação por NaHCO3 60 mM")// e 119 mM ")
  val motilidade = NodeSpec("as maiores taxas de motilidade espermática")

    behavior of "TestPortugueseDoc2 Paragraph 3"

    passingTest should "have correct edges 1" taggedAs(George) in {
      val tester = new GraphTester(text)
      tester.test(EdgeSpec(ativacao, Causal, motilidade)) should be (successful)
    }
  }
  */
}
