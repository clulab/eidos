package org.clulab.wm.eidos.text.eval6

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text._

class TestDoc2 extends Test {

  { // Paragraph 1
    val text = """
                 |Up to 4.8 million people in South Sudan--well over one-third of the
                 |population--will be facing severe food shortages over the coming months, and the risk of a
                 |hunger catastrophe continues to threaten parts of the country, three UN agencies warned
                 |today.
               """
    val tester = new Tester(text)

    val food = NodeSpec("food", Dec("shortages", "severe"), Quant("severe"))
    val hunger = NodeSpec("hunger", Dec("catastrophe"))

    behavior of "TestDoc2 Paragraph 1"

    passingTest should "have correct singleton node 1" taggedAs(Mithun) in {
      tester.test(food) should be(successful)
    }

    passingTest should "have correct singleton node 2" taggedAs(Mithun) in {
      tester.test(hunger) should be (successful)
    }
  }

  { // Paragraph 2
    val text = """
                 |The Food and Agriculture Organization of the United Nations (FAO), the United Nations
                 |Children's Fund (UNICEF) and the World Food Programme (WFP) stressed that while the
                 |deteriorating situation coincides with an unusually long and harsh annual lean season, when families
                 |have depleted their food stocks and new harvests are not expected until August, the level of food
                 |insecurity this year is unprecedented.
               """
    val tester = new Tester(text)

    val situation = NodeSpec("situation", Dec("deteriorating"))
    val leanSeason = NodeSpec("annual lean season", Quant("long", "unusually"), Quant("harsh"))
    val foodStocks = NodeSpec("food stocks", Dec("depleted"))
    val foodInsecurity = NodeSpec("level of food insecurity this year", Quant("unprecedented"))

    behavior of "TestDoc2 Paragraph 2"

    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(situation) should be(successful)
    }

      //this is passing from mithun's side, but will pass once zheng fixes the "merging entities bug"
    failingTest should "have correct singleton node 2" taggedAs(Mithun) in {
      tester.test(leanSeason) should be(successful)
    }

    passingTest should "have correct singleton node 3" taggedAs(Mithun) in {
      tester.test(foodStocks) should be(successful)
    }

    passingTest should "have correct singleton node 4" taggedAs(Mithun) in {
      tester.test(foodInsecurity) should be(successful)
    }

    //this is dependant on the lean season thing above. i.e passing from mithun's side, but will pass once zheng fixes the "merging entities bug"
    failingTest should "have correct edge 1" taggedAs(Mithun) in {
      tester.test(EdgeSpec(situation, Correlation, leanSeason)) should be(successful)
    }

    // TODO: must test for the relation "families" => "food stocks" NOT being present!
  }

  { // Paragraph 3
    val text = """
                 |This is the highest level of hunger since the conflict in South
                 |Sudan began two-and-a-half years ago.
               """
    val tester = new Tester(text)

    val hunger = NodeSpec("level of hunger", Quant("highest"))

    behavior of "TestDoc2 Paragraph 3"

    failingTest should "have correct singleton node 1" taggedAs(Mithun) in {
      tester.test(hunger) should be (successful)
    }
  }

  { // Paragraph 4
    val text = """
                 |"We are very worried to see that food insecurity is spreading beyond conflict areas as rising prices,
                 |impassable roads and dysfunctional markets are preventing many families, even those in towns and
                 |cities, from accessing food," said FAO Country Representative Serge Tissot.
               """
    val tester = new Tester(text)

    val prices = NodeSpec("prices", Inc("rising"))
    val roads = NodeSpec("roads", Quant("impassable"))
    val markets = NodeSpec("markets", Quant("dysfunctional"))
    val families = NodeSpec("families", Quant("many"), Dec("preventing"))

    behavior of "TestDoc2 Paragraph 4"

    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(prices) should be(successful)
    }

    passingTest should "have correct singleton node 2" taggedAs(Fan) in {
      tester.test(roads) should be(successful)
    }

    passingTest should "have correct singleton node 3" taggedAs(Fan) in {
      tester.test(markets) should be(successful)
    }

    passingTest should "have correct singleton node 4" taggedAs(Fan) in {
      tester.test(families) should be(successful)
    }

    // If the processor improves and is better able to handle long coordinations/conj, then maybe we can get this.
    // Currently no path exists.
    futureWorkTest should "have correct edge 1" taggedAs(Fan) in {
      tester.test(EdgeSpec(prices, Causal, families)) should be(successful)
    }

    passingTest should "have correct edge 2" taggedAs(Fan) in {
      tester.test(EdgeSpec(roads, Causal, families)) should be(successful)
    }

    passingTest should "have correct edge 3" taggedAs(Fan) in {
      tester.test(EdgeSpec(markets, Causal, families)) should be (successful)
    }
  }

  { // Paragraph 5
    val text = """
                 |Food insecurity and conflict are also forcing many families to leave South Sudan for neighbouring countries.
               """
    val tester = new Tester(text)

    val foodInsecurity = NodeSpec("Food insecurity")
    val conflict = NodeSpec("conflict")
    val fams = NodeSpec("families to leave South Sudan for neighbouring countries", Quant("many"))

    behavior of "TestDoc2 Paragraph 5"

    // Until we allow VPs in entityFinder, the causal events aren't found, so these 2 nodes are pruned.
    futureWorkTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(foodInsecurity) should be(successful)
    }

    // Until we allow VPs in entityFinder, the causal events aren't found, so these 2 nodes are pruned.
    futureWorkTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(conflict) should be(successful)
    }

    //this is dependant on something becky is working on. arbitrary predicates thing
    failingTest should "have correct singleton node 3" taggedAs(Mithun) in {
      tester.test(fams) should be(successful)
    }

    //this is dependant on something becky is working on. arbitrary predicates thing
    failingTest should "have correct edge 1" taggedAs(Mithun) in {
      tester.test(EdgeSpec(foodInsecurity, Causal, fams)) should be(successful)
    }

    //this is dependant on something becky is working on. arbitrary predicates thing
    failingTest should "have correct edge 2" taggedAs(Mithun) in {
      tester.test(EdgeSpec(conflict, Causal, fams)) should be (successful)
    }
  }

  { // Paragraph 6
    val text = """
                 |"The levels of malnutrition among children continue to be truly alarming," said Mahimbo Mdoe,
                 |UNICEF's Representative in South Sudan.
               """
    val tester = new Tester(text)

    // removed 'truly' from this test and quantifiers.tsv bc it's not gradable
    val malnutrition = NodeSpec("levels of malnutrition", Quant("alarming"))

    behavior of "TestDoc2 Paragraph 6"

    passingTest should "have correct singleton node 1" taggedAs(Fan) in {
      tester.test(malnutrition) should be (successful)
    }
  }

  { // Paragraph 7
    val text = """
                 |We have started ramping up food and nutrition support, but much more is needed to keep things
                 |from deteriorating even further during the lean season," said WFP Country Director Joyce Luma.
               """
    val tester = new Tester(text)

    val food = NodeSpec("food", Inc("ramping up"), Inc("support"))
    val nutrition = NodeSpec("nutrition", Inc("ramping up"), Inc("support"))

    behavior of "TestDoc2 Paragraph 7"

    passingTest should "have correct singleton node 1" taggedAs(Fan) in {
      tester.test(food) should be (successful)
    }

    passingTest should "have correct singleton node 2" taggedAs(Fan) in {
      tester.test(nutrition) should be (successful)
    }
  }

  { // Paragraph 8
    val text = """
                 |"We are now seeing sharp spikes of need in new areas, such as Eastern Equatoria or Western
                 |Bahr el-Ghazal, where malnutrition rates in some places are reaching dangerous levels.
               """
    val tester = new Tester(text)

    val need = NodeSpec("need in new areas", Inc("spikes", "sharp"))
    val malnutrition = NodeSpec("malnutrition rates in some places", Quant("dangerous")) // TODO: must decide what to do with "levels"

    behavior of "TestDoc2 Paragraph 8"

    passingTest should "have correct singleton node 1" taggedAs(Fan) in {
      tester.test(need) should be(successful)
    }

    passingTest should "have correct singleton node 2" taggedAs(Fan) in {
      tester.test(malnutrition) should be (successful)
    }
  }

  { // Paragraph 9
    val text = """
                 |The dramatic rise in malnutrition rates, means that in the first four months of the year UNICEF
                 |has already treated 45 per cent of its planned 2016 caseload of 166,000 children.
               """
    val tester = new Tester(text)

    val malnutrition = NodeSpec("malnutrition rates", Inc("rise", "dramatic"))

    behavior of "TestDoc2 Paragraph 9"

    failingTest should "have correct singleton node 1" taggedAs(Vikas) in {
      tester.test(malnutrition) should be (successful)
    }
  }

}
