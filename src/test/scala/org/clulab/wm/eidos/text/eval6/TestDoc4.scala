package org.clulab.wm.eidos.text.eval6

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.{Causal, Correlation, IsA}
import org.clulab.wm.eidos.text.Dec
import org.clulab.wm.eidos.text.EdgeSpec
import org.clulab.wm.eidos.text.Inc
import org.clulab.wm.eidos.text.NodeSpec
import org.clulab.wm.eidos.text.Quant

class TestDoc4 extends Test {
  
  { // Paragraph 1
    val text = """
According to the latest IPC analysis, famine conditions, previously
reported in February 2017 in former Leer and Mayendit counties in
former Unity State, were no longer occurring by late June 2017.
Overall, the number of people facing IPC Phase 5: "Catastrophe" food
security conditions declined from over 100 000 in February to about
45 000 in June due to sustained multi-sectoral humanitarian
assistance operations and the two counties are currently classified
as IPC Phase 4: "Emergency".
      """
  
    val tester = new Tester(text)

    // Sentence 1
    val conditions1 = NodeSpec("famine conditions", Dec("no longer occuring"))
    
    // Sentence 2
    // Note that the quotes are automatically smarted during processing.  The colon probably wrecks the parse.
    val conditions2 = NodeSpec(/*"the number of people facing IPC Phase 5: " +*/ "``Catastrophe'' food security conditions", Dec("declined"))
    val fromQuant = NodeSpec("``Catastrophe'' food security conditions", Quant("100 000", "over"))
    val toQuant = NodeSpec("``Catastrophe'' food security conditions", Quant("45 000"))
    val operations = NodeSpec("sustained multi-sectoral humanitarian assistance operations") // TODO: Is "sustained" a quantification?
    
    behavior of "TestDoc4 Paragraph 1"

    failingTest should "have correct node 1" taggedAs(Somebody) in {
      tester.test(conditions1) should be (successful)
    }
    failingTest should "have correct node 2" taggedAs(Somebody) in {
      tester.test(fromQuant) should be (successful)
    }
    failingTest should "have correct node 3" taggedAs(Somebody) in {
      tester.test(toQuant) should be (successful)
    }
    
    passingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(operations, Causal, conditions2)) should be (successful)
    }
  }
  
  { // Paragraph 2
    val text = """
However, nationwide, the food insecure caseload (IPC Phases 3, 4 and 5)
increased from about 5 million in February to a record high of 6
million in June as food access continues to be severely constrained by
widespread insecurity, large scale displacements, high food prices,
market disruptions, macro-economic collapse and exhaustion of
households' coping mechanisms. The areas of major concern are Greater
Jonglei and Unity states, where over 60 percent of the population faces
"Crisis", "Emergency" and "Catastrophe" levels of food insecurity. In
particular, the people facing catastrophic conditions are located in
Ayod County in Greater Jonglei State and in Leer, Koch and Mayendit
counties in Unity State.
      """
  
    val tester = new Tester(text)

    // Sentence 1
    val caseload = NodeSpec("food insecure caseload", Inc("increased"))
    val fromQuant = NodeSpec("food insecure caseload", Quant("5 million"))
    val toQuant = NodeSpec("food insecure caseload", Quant("6 million"))
    val access = NodeSpec("food access", Quant("constrained", "severely"))
    val insecurity = NodeSpec("insecurity", Quant("widespread"))
    val displacements = NodeSpec("displacements", Quant("large scale"))
    val foodPrices = NodeSpec("food prices", Quant("high"))
    val marketDisruptions = NodeSpec("market disruptions")
    val collapse = NodeSpec("macro-economic collapse")
    val mechanisms = NodeSpec("exhaustion of households' coping mechanisms")
    
    // Sentence 2
    val concern = NodeSpec("concern", Quant("major"))
    val location = NodeSpec("Greater Jonglei and Unity states")
    val foodInsecurity = NodeSpec("food insecurity", Quant("Crisis levels"), Quant("Emergency levels"), Quant("Catastrophe levels"))
    
    // Sentence 3
    val conditions = NodeSpec("conditions", Quant("catastrophic"))
    
    behavior of "TestDoc4 Paragraph 2"

    failingTest should "have correct node 1" taggedAs(Somebody) in {
      tester.test(fromQuant)
    }
    failingTest should "have correct node 2" taggedAs(Somebody) in {
      tester.test(toQuant)
    }
    failingTest should "have correct node 3" taggedAs(Somebody) in {
      tester.test(concern)
    }
    failingTest should "have correct node 4" taggedAs(Somebody) in {
      tester.test(location)
    }
    failingTest should "have correct node 5" taggedAs(Somebody) in {
      tester.test(foodInsecurity)
    }
    failingTest should "have correct node 6" taggedAs(Somebody) in {
      tester.test(conditions)
    }
    
    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(caseload, Correlation, access))
    }
    failingTest should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(access, Causal, insecurity))
    }
    failingTest should "have correct edges 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(access, Causal, displacements))
    }
    failingTest should "have correct edges 4" taggedAs(Somebody) in {
      tester.test(EdgeSpec(access, Causal, foodPrices))
    }
    failingTest should "have correct edges 5" taggedAs(Somebody) in {
      tester.test(EdgeSpec(access, Causal, marketDisruptions))
    }
    failingTest should "have correct edges 6" taggedAs(Somebody) in {
      tester.test(EdgeSpec(access, Causal, collapse))
    }
    failingTest should "have correct edges 7" taggedAs(Somebody) in {
      tester.test(EdgeSpec(access, Causal, mechanisms))
    }
    failingTest should "have correct edges 8" taggedAs(Somebody) in {
      tester.test(EdgeSpec(concern, IsA, location))
    }
  }

  { // Paragraph 3
    val text = """
Since the start of the conflict in mid-December 2013, about 3.9
million people were forced to flee their homes due to insecurity,
including about 1.9 million IDPs and 2 million that sought refuge
in neighbouring countries (Uganda, the Sudan, the Democratic
Republic of the Congo, Ethiopia and Kenya).
      """
  
    val tester = new Tester(text)

    val flee = NodeSpec("flee their homes") // forced to is Transparent Link?
    val insecurity = NodeSpec("insecurity")
    val soughtRefuge = NodeSpec("sought refuge")

    behavior of "TestDoc4 Paragraph 3"

    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, flee))
    }
    failingTest should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, soughtRefuge))
    }
  }

  { // Paragraph 4
    val text = """
In southern bi-modal rainfall areas, harvesting of first season
crops was concluded in August. Seasonal rains were above-average
in the "green belt", including the former Central and
Western Equatoria states, while in the former Eastern Equatoria
State they started in late April with about a one-month delay. In
northern and central uni-modal rainfall areas, harvesting of short
cycle sorghum and maize crops has recently started, while long
cycle sorghum crops will be gathered from November to January.
Weather conditions have been generally favourable so far as
seasonal rains have been average to above average, thus
benefiting vegetation conditions.
      """
  
    val tester = new Tester(text)

    // Sentence 1
    val harvesting = NodeSpec("harvesting", Dec("concluded"))
    
    // Sentence 2
    val rains1 = NodeSpec("Seasonal rains", Quant("above average"))
    val rains2 = NodeSpec("Seasonal rains", Inc("started"))
    
    // Sentence 3
    val harvesting2 = NodeSpec("harvesting", Inc("started", "recently"))
    
    // Sentence 4
    val conditions = NodeSpec("Weather conditions", Quant("favorable", "generally"))
    val rains3 = NodeSpec("seasonal rains", Quant("average to above average"))
    val benefiting = NodeSpec("vegetation conditions", Inc("benefiting"))
    
    behavior of "TestDoc4 Paragraph 4"

    failingTest should "have correct node 1" taggedAs(Somebody) in {
      tester.test(rains1)
    }
    failingTest should "have correct node 2" taggedAs(Somebody) in {
      tester.test(rains2)
    }
    failingTest should "have correct node 3" taggedAs(Somebody) in {
      tester.test(harvesting2)
    }
    failingTest should "have correct node 4" taggedAs(Somebody) in {
      tester.test(conditions)
    }
    
    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rains3, Causal, benefiting))
    }
  }

  { // Paragraph 5
    val text = """
However, prospects for 2017 aggregate cereal production are
generally unfavourable as agricultural activities continue to be
severely affected by the protracted and widespread insecurity,
which is constraining farmers' access to fields and is causing
large scale displacement of people, input shortages and damage
to households' productive assets. In the traditionally surplus-producing
areas of southern Greater Equatoria Region, crop
production is expected to be lower than the already poor 2016
output due to recent massive displacements outside the former
Central and Eastern Equatoria states. Notably, about 75 percent
of the population of the former Central Equatoria State has
reportedly left their living areas.
      """
  
    val tester = new Tester(text)

    // Sentence 1
    val prospects = NodeSpec("prospects for 2017 aggregate cereal production", Quant("unfavourable", "generally"))
    val insecurity = NodeSpec("insecurity", Quant("protracted"), Quant("widespread"))
    val access = NodeSpec("access to fields")
    val displacement = NodeSpec("displacement", Quant("large scale"))
    val shortages = NodeSpec("input shortages")
    val damage = NodeSpec("damage")

    // Sentence 2
    val production = NodeSpec("crop production", Dec("lower"))
    val displacements = NodeSpec("displacements", Quant("massive"))
    
    // Sentence 3
    val population = NodeSpec("population of the former Central Equatoria State", Quant("75 percent"))
    
    behavior of "TestDoc4 Paragraph 5"

    failingTest should "have correct nodes 1" taggedAs(Somebody) in {
      tester.test(population)
    }

    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      EdgeSpec(insecurity, Causal, prospects)
    }
    failingTest should "have correct edges 2" taggedAs(Somebody) in {
      EdgeSpec(insecurity, Causal, access)
    }
    failingTest should "have correct edges 3" taggedAs(Somebody) in {
      EdgeSpec(insecurity, Causal, displacement)
    }
    failingTest should "have correct edges 4" taggedAs(Somebody) in {
      EdgeSpec(insecurity, Causal, shortages)
    }
    failingTest should "have correct edges 5" taggedAs(Somebody) in {
      EdgeSpec(insecurity, Causal, damage)
    }
    failingTest should "have correct edges 6" taggedAs(Somebody) in {
      EdgeSpec(displacements, Causal, production)
    }
}

  { // Paragraph 6
    val text = """
In addition, Fall Armyworm infestations have been reported in all
regions of the country, with significant crop damage, especially in
parts of former Northern Bahr el Ghazal, Eastern Equatoria and
Central Equatoria states.
      """

    val tester = new Tester(text)

    val infestations = NodeSpec("Fall Armyworm infestations")
    val regions = NodeSpec("regions of the country", Quant("all"))
    val damage = NodeSpec("crop damage", Quant("significant"))
    
    behavior of "TestDoc4 Paragraph 6"

    failingTest should "have correct nodes 1" taggedAs(Somebody) in {
      tester.test(regions)
    }
    
    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(infestations, Correlation, damage))
    }
  }

  { // Paragraph 7
    val text = """
In the capital, Juba, prices of maize and sorghum more than
doubled in the first semester of 2017, reaching record levels in
June, driven by a tight supply situation, market disruptions,
hyperinflation and a significant depreciation of the local currency.
Subsequently, they declined by about 12 percent between June
and August, following the first season harvest in southern bimodal
rainfall areas and the establishment, by the Government,
of a trading company selling basic food commodities at
subsidized prices. Prices of groundnuts decreased by 22 percent
over the same period, while prices of wheat flour continued to
soar in recent months, reaching new record highs in August.
Overall, prices of these food staples in August were more than
twice the high levels in August last year and up to 12 times higher
than in the corresponding period two years earlier.
      """
  
    val tester = new Tester(text)

    // Sentence 1
    val prices = NodeSpec("prices of maize and sorghum", Inc("doubled", "more than"))
    val pricesTo = NodeSpec("prices of maize and sorghum", Quant("record levels"))
    val situation = NodeSpec("tight supply situation")
    val disruptions = NodeSpec("market disruptions")
    val hyperinflation = NodeSpec("hyperinflation")
    val depreciation = NodeSpec("depreciation of the local currency", Dec("significant"))
    
    // Sentence 2
    val they = NodeSpec("they", Dec("declined", "by about 12 percent")) // coreference?
    val harvest = NodeSpec("first season harvest")
    val selling = NodeSpec("selling at subsidized prices")
    
    // Sentence 3
    val prices2 = NodeSpec("prices of groundnuts", Dec("decreased", "by 22 percent"))
    val prices3 = NodeSpec("prices of wheat flour", Quant("soar"))
    val prices3To = NodeSpec("prices of wheat flour", Quant("new record highs"))
    
    // Sentence 4
    val prices4 = NodeSpec("prices of these food staples", Quant("twice", "more than"))
    val prices5 = NodeSpec("prices of these food staples", Quant("12 times higher"))
    
    behavior of "TestDoc4 Paragraph 7"

    failingTest should "have correct node 1" taggedAs(Somebody) in {
      tester.test(pricesTo)
    }
    failingTest should "have correct node 2" taggedAs(Somebody) in {
      tester.test(situation)
    }
    failingTest should "have correct node 3" taggedAs(Somebody) in {
      tester.test(prices2)
    }
    failingTest should "have correct node 4" taggedAs(Somebody) in {
      tester.test(prices3)
    }
    failingTest should "have correct node 4" taggedAs(Somebody) in {
      tester.test(prices3To)
    }
    failingTest should "have correct node 5" taggedAs(Somebody) in {
      tester.test(prices4)
    }
    failingTest should "have correct node 6" taggedAs(Somebody) in {
      tester.test(prices5)
    }

    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      EdgeSpec(situation, Causal, prices)
    }
    failingTest should "have correct edges 2" taggedAs(Somebody) in {
      EdgeSpec(disruptions, Causal, prices)
    }
    failingTest should "have correct edges 3" taggedAs(Somebody) in {
      EdgeSpec(hyperinflation, Causal, prices)
    }
    failingTest should "have correct edges 4" taggedAs(Somebody) in {
      EdgeSpec(depreciation, Causal, prices)
    }
    failingTest should "have correct edges 5" taggedAs(Somebody) in {
      EdgeSpec(harvest, Correlation, they)
    }
    failingTest should "have correct edges 6" taggedAs(Somebody) in {
      EdgeSpec(selling, Correlation, they)
    }
  }
}
