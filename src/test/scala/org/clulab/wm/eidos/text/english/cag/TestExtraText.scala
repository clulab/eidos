package org.clulab.wm.eidos.text.english.cag

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.EnglishTest
import org.clulab.wm.eidos.test.TestUtils.{Somebody, successful}

class TestExtraText extends EnglishTest {
  { // Paragraph 1
    val text = """
                 |Second , the sharp rise in the international price of oil in 2007
                 |resulted in a significant drain on foreign reserves .
                 |Anything which impedes this is bound to lead to tyranny and the
                 |curtailment of the rights enshrined in the constitution .
                 |To sum up , Ethiopia has to expand its business diplomacy horizons
                 |and promote investment opportunities than ever before to alleviate
                 |poverty and establish globally competitive economy .
                 |He said in order to boost intra-African trade , tariffs needed to
                 |be brought down .
      """

    val tester = new GraphTester(text)

    val oil = NodeSpec("international price of oil", Inc("rise", "sharp"), TimEx("2007"))
    val reserves = NodeSpec("foreign reserves", Dec("drain", "significant"))
    val rights = NodeSpec("curtailment of the rights enshrined in the constitution", Dec("curtailment"))
    val investment = NodeSpec("investment opportunities", Inc("promote"))
    val poverty = NodeSpec("alleviate poverty", Pos("alleviate"))
    val trade = NodeSpec("intra-African trade", Inc("boost"))
    val tariffs = NodeSpec("tariffs", Dec("brought down"))


    behavior of "inc/dec triggers test"

    passingTest should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(oil, Causal, reserves)) should be (successful)
    }
    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(rights) should be (successful)
    }
    passingTest should "have correct edge 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(investment, Causal, poverty)) should be (successful)
    }
    passingTest should "have correct edge 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(tariffs, Causal, trade)) should be (successful)
    }
  }

  {
    // Paragraph 2
    val text = """These higher productivity birds have the potential to increase household income
                 |and provide sources of animal protein for consumption.
                 |Overall, we find that purchasing improved chickens results in a statistically
                 |significant increase in income from egg and chicken sales compared to households
                 |that own only local breeds of chicken.
      """

    val tester = new GraphTester(text)

    val birds = NodeSpec("higher productivity birds", Inc("higher"))
    val income = NodeSpec("household income", Inc("increase"))
    val protein = NodeSpec("sources of animal protein for consumption", Inc("provide"))
    val chickens = NodeSpec("purchasing improved chickens", Pos("improved"))
    val sales = NodeSpec("income from egg and chicken sales", Inc("increase", "significant"))

    behavior of "p2-extra"

    passingTest should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(birds, Causal, income)) should be (successful)
    }
    failingTest should "have correct edge 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(birds, Causal, protein)) should be (successful)
    }
    passingTest should "have correct edge 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(chickens, Causal, sales)) should be (successful)
    }

  }

}
