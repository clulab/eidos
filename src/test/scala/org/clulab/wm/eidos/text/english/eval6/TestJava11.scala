package org.clulab.wm.eidos.text.english.eval6

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.EnglishTest
import org.clulab.wm.eidos.test.TestUtils._

class TestJava11 extends EnglishTest {

  { // Document 2, Paragraph 2
    val text = """
                 |The Food and Agriculture Organization of the United Nations (FAO), the United Nations
                 |Children's Fund (UNICEF) and the World Food Programme (WFP) stressed that while the
                 |deteriorating situation coincides with an unusually long and harsh annual lean season, when families
                 |have depleted their food stocks and new harvests are not expected until August, the level of food
                 |insecurity this year is unprecedented.
               """
    val tester = new GraphTester(text)

    val situation = NodeSpec("deteriorating situation", Dec("deteriorating"))
    val leanSeason = NodeSpec("unusually long and harsh annual lean season", Quant("harsh"), TimEx("annual"))

    behavior of "TestDoc2 Paragraph 2"

    passingTest should "have correct edge 1" taggedAs(Keith) in {
      tester.test(EdgeSpec(situation, Correlation, leanSeason)) should be(successful)
    }
  }
}
