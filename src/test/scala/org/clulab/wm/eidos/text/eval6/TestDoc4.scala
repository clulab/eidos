package org.clulab.wm.eidos.text.eval6

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.{Causal, Dec, EdgeSpec, NodeSpec}

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
  
    // Note that the quotes are automatically smarted during processing.
    val conditions = NodeSpec(/*"the number of people facing IPC Phase 5: " +*/ "``Catastrophe'' food security conditions", Dec("declined"))
    val operations = NodeSpec("sustained multi-sectoral humanitarian assistance operations")
    
    behavior of "TestDoc4 Paragraph 1"

    passingTest should "have correct edges 1" taggedAs(Keith) in {
      val tester = new Tester(text)
  
      tester.test(EdgeSpec(operations, Causal, conditions)) should be (successful)
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
  
    val concerns = NodeSpec("Concerns about insufficient food access") // Make some node spec
    
    behavior of "TestDoc4 Paragraph 2"

    ignore should "have correct edges 1" taggedAs(Keith) in {
      val tester = new Tester(text)
  
//      tester.test(EdgeSpec(concerns, Causal, concerns)) should be (successful) // Test edges connecting them
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
  
    val concerns = NodeSpec("Concerns about insufficient food access") // Make some node spec
    
    behavior of "TestDoc4 Paragraph 3"

    ignore should "have correct edges 1" taggedAs(Keith) in {
      val tester = new Tester(text)
  
//      tester.test(EdgeSpec(concerns, Causal, concerns)) should be (successful) // Test edges connecting them
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
  
    val concerns = NodeSpec("Concerns about insufficient food access") // Make some node spec
    
    behavior of "TestDoc4 Paragraph 4"

    ignore should "have correct edges 1" taggedAs(Keith) in {
      val tester = new Tester(text)
  
//      tester.test(EdgeSpec(concerns, Causal, concerns)) should be (successful) // Test edges connecting them
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
  
    val concerns = NodeSpec("Concerns about insufficient food access") // Make some node spec
    
    behavior of "TestDoc4 Paragraph 5"

    ignore should "have correct edges 1" taggedAs(Keith) in {
      val tester = new Tester(text)
  
//      tester.test(EdgeSpec(concerns, Causal, concerns)) should be (successful) // Test edges connecting them
    }
  }

  { // Paragraph 6
    val text = """
In addition, Fall Armyworm infestations have been reported in all
regions of the country, with significant crop damage, especially in
parts of former Northern Bahr el Ghazal, Eastern Equatoria and
Central Equatoria states.
      """
  
    val concerns = NodeSpec("Concerns about insufficient food access") // Make some node spec
    
    behavior of "TestDoc4 Paragraph 6"

    ignore should "have correct edges 1" taggedAs(Keith) in {
      val tester = new Tester(text)
  
//      tester.test(EdgeSpec(concerns, Causal, concerns)) should be (successful) // Test edges connecting them
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
  
    val concerns = NodeSpec("Concerns about insufficient food access") // Make some node spec
    
    behavior of "TestDoc4 Paragraph 7"

    ignore should "have correct edges 1" taggedAs(Keith) in {
      val tester = new Tester(text)
  
//      tester.test(EdgeSpec(concerns, Causal, concerns)) should be (successful) // Test edges connecting them
    }
  }
}
