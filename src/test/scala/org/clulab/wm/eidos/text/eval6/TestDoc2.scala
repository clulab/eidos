package org.clulab.wm.eidos.text.eval6

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text._

class TestDoc2 extends Test {
  
  { // Paragraph 1
    val text = """
                 |Up to 4.8 million people in South Sudan – well over one-third of the
                 |population – will be facing severe food shortages over the coming months, and the risk of a
                 |hunger catastrophe continues to threaten parts of the country, three UN agencies warned
                 |today.
               """
    val tester = new Tester(text)

    behavior of "TestDoc2 Paragraph 1"

    ignore should "have correct edges 1" taggedAs(Somebody) in {
      val food = NodeSpec("food", Dec("shortages", "severe"))
      val hunger = NodeSpec("hunger", Dec("catastrophe"))

      tester.test(food) should be (successful)
      tester.test(hunger) should be (successful)
    }
  }

  { // Paragraph 2
    val text = """
                 |The Food and Agriculture Organization of the United Nations (FAO), the United Nations
                 |Children’s Fund (UNICEF) and the World Food Programme (WFP) stressed that while the
                 |deteriorating situation coincides with an unusually long and harsh annual lean season, when families
                 |have depleted their food stocks and new harvests are not expected until August, the level of food
                 |insecurity this year is unprecedented.
               """
    val tester = new Tester(text)

    behavior of "TestDoc2 Paragraph 2"

    ignore should "have correct edges 2" taggedAs(Somebody) in {
      val situation = NodeSpec("situation", Dec("deteriorating"))
      val leanSeason = NodeSpec("annual lean season", Quant("unusually long", "harsh"))
      val foodStocks = NodeSpec("food stocks", Dec("depleted"))
      val foodInsecurity = NodeSpec("food insecurity", Quant("unprecedented"))

      tester.test(situation) should be (successful)
      tester.test(leanSeason) should be (successful)
      tester.test(foodStocks) should be (successful)
      tester.test(foodInsecurity) should be (successful)
      tester.test(EdgeSpec(situation, Correlation, leanSeason)) should be (successful)
    }
  }
}
