package org.clulab.wm.eidos.text.eval6

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text._

class TestDoc2 extends Test {
  
  { // Paragraph 1
    val text = """
                 |Up to 4.8 million people in South Sudan - well over one-third of the
                 |population - will be facing severe food shortages over the coming months, and the risk of a
                 |hunger catastrophe continues to threaten parts of the country, three UN agencies warned
                 |today.
               """
    val tester = new Tester(text)

    behavior of "TestDoc2 Paragraph 1"

    ignore should "have correct output #1" taggedAs(Somebody) in {
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

    ignore should "have correct output #2" taggedAs(Somebody) in {
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

  { // Paragraph 3
    val text = """
                 |This is the highest level of hunger since the conflict in South
                 |Sudan began two-and-a-half years ago.
               """
    val tester = new Tester(text)

    behavior of "TestDoc2 Paragraph 3"

    ignore should "have correct output #3" taggedAs(Somebody) in {
      val hunger = NodeSpec("hunger", Quant("highest"))

      tester.test(hunger) should be (successful)
    }
  }

  { // Paragraph 4
    val text = """
                 |“We are very worried to see that food insecurity is spreading beyond conflict areas as rising prices,
                 |impassable roads and dysfunctional markets are preventing many families, even those in towns and
                 |cities, from accessing food,” said FAO Country Representative Serge Tissot.
               """
    val tester = new Tester(text)

    behavior of "TestDoc2 Paragraph 4"

    ignore should "have correct output #4" taggedAs(Somebody) in {
      // TODO
    }
  }

  { // Paragraph 5
    val text = """
                 |Food insecurity and conflict are also forcing many families to leave South Sudan for neighbouring countries.
               """
    val tester = new Tester(text)

    behavior of "TestDoc2 Paragraph 5"

    ignore should "have correct output #5" taggedAs(Somebody) in {
      // TODO
    }
  }

  { // Paragraph 6
    val text = """
                 |“The levels of malnutrition among children continue to be truly alarming,” said Mahimbo Mdoe,
                 |UNICEF’s Representative in South Sudan.
               """
    val tester = new Tester(text)

    behavior of "TestDoc2 Paragraph 6"

    ignore should "have correct output #6" taggedAs(Somebody) in {
      // TODO
    }
  }

  { // Paragraph 7
    val text = """
                 |We have started ramping up food and nutrition support, but much more is needed to keep things
                 |from deteriorating even further during the lean season,” said WFP Country Director Joyce Luma.
               """
    val tester = new Tester(text)

    behavior of "TestDoc2 Paragraph 7"

    ignore should "have correct output #7" taggedAs(Somebody) in {
      // TODO
    }
  }

  { // Paragraph 8
    val text = """
                 |“We are now seeing sharp spikes of need in new areas, such as Eastern Equatoria or Western
                 |Bahr el-Ghazal, where malnutrition rates in some places are reaching dangerous levels.
               """
    val tester = new Tester(text)

    behavior of "TestDoc2 Paragraph 8"

    ignore should "have correct output #8" taggedAs(Somebody) in {
      // TODO
    }
  }

  { // Paragraph 9
    val text = """
                 |The dramatic rise in malnutrition rates, means that in the first four months of the year UNICEF
                 |has already treated 45 per cent of its planned 2016 caseload of 166,000 children.
               """
    val tester = new Tester(text)

    behavior of "TestDoc2 Paragraph 9"

    ignore should "have correct output #9" taggedAs(Somebody) in {
      // TODO
    }
  }

}
