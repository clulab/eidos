package org.clulab.wm.eidos.text.english.eval6

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.graph._

class TestDoc4 extends EnglishTest {
  
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
  
    val tester = new GraphTester(text)

    // Sentence 1
    val conditions1 = NodeSpec("famine conditions", Dec("no longer occurring"), TimEx("February 2017"), TimEx("June 2017"))
    
    // Sentence 2
    // Note that the quotes are automatically smarted during processing.  The colon probably wrecks the parse.
    val conditions2 = NodeSpec(""""Catastrophe" food security conditions""", Dec("declined"), TimEx("February"), TimEx("June"))
    val operations = NodeSpec("sustained multi-sectoral humanitarian assistance operations") // TODO: Is "sustained" a quantification?
    
    behavior of "TestDoc4 Paragraph 1"

    passingTest should "have correct node 1" taggedAs(Vikas) in {
      tester.test(conditions1) should be (successful)
    }
    // Becky: removed for now.  I don't know that we will be capturing explicit quantities, and if so how we will represent them.
//    futureWorkTest should "have correct node 2" taggedAs(Vikas) in {
//      tester.test(fromQuant) should be (successful)
//    }
//    futureWorkTest should "have correct node 3" taggedAs(Vikas) in {
//      tester.test(toQuant) should be (successful)
//    }
    // The syntax is hopelessly broken, we can removed some of the inner punctuation to try to fix or leave as is.
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

    val tester = new GraphTester(text)

    // Sentence 1
    val caseload = NodeSpec("the food insecure caseload", Inc("increased"), TimEx("February"))
    val access = NodeSpec("food access", Quant("constrained", "severely"))
    val insecurity = NodeSpec("widespread insecurity", Inc("widespread"))
    val displacements = NodeSpec("large scale displacements", Quant("large"))
    val foodPrices = NodeSpec("high food prices", Quant("high"), Inc("high"))
    val marketDisruptions = NodeSpec("market disruptions", Dec("disruptions"))
    val collapse = NodeSpec("macro-economic collapse", Dec("collapse"))
    val mechanisms = NodeSpec("exhaustion of households' coping mechanisms", Dec("exhaustion"))

    // Sentence 2
    val concern = NodeSpec("concern", Quant("major"))
    val location = NodeSpec("Greater Jonglei and Unity states")
    val foodInsecurity = NodeSpec("food insecurity", Quant("Crisis levels"), Quant("Emergency levels"), Quant("Catastrophe levels"))

    // Sentence 3
    val conditions = NodeSpec("conditions", Quant("catastrophic"))

    behavior of "TestDoc4 Paragraph 2"

    passingTest should "have correct node 1" taggedAs(Somebody) in {
      tester.test(caseload) should be (successful)
    }
    passingTest should "have correct node 3" taggedAs(Somebody) in {
      tester.test(concern) should be (successful)
    }
    failingTest should "have correct node 4" taggedAs(Somebody) in {
      tester.test(location) should be (successful)
    }
    // we don't currently handle this type of Quantification
    futureWorkTest should "have correct node 5" taggedAs(Somebody) in {
      tester.test(foodInsecurity) should be (successful)
    }
    // we don't currently handle this type of sentiment/quantification
    futureWorkTest should "have correct node 6" taggedAs(Somebody) in {
      tester.test(conditions) should be (successful)
    }

    // here, the only "trigger" is "as", which is too ambiguous in general
    futureWorkTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(caseload, Correlation, access)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(access, Causal, insecurity)) should be (successful)
    }
    passingTest should "have correct edges 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(access, Causal, displacements)) should be (successful)
    }
    passingTest should "have correct edges 4" taggedAs(Somebody) in {
      tester.test(EdgeSpec(access, Causal, foodPrices)) should be (successful)
    }
    passingTest should "have correct edges 5" taggedAs(Somebody) in {
      tester.test(EdgeSpec(access, Causal, marketDisruptions)) should be (successful)
    }
    passingTest should "have correct edges 6" taggedAs(Somebody) in {
      tester.test(EdgeSpec(access, Causal, collapse)) should be (successful)
    }
    passingTest should "have correct edges 7" taggedAs(Somebody) in {
      tester.test(EdgeSpec(access, Causal, mechanisms)) should be (successful)
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

    val tester = new GraphTester(text)

    val flee = NodeSpec("3.9 million people were forced to flee their homes") // forced to is Transparent Link?
    val insecurity = NodeSpec("insecurity")

    behavior of "TestDoc4 Paragraph 3"

    // it would be nice to *sometimes* license traversing an incoming nsubj during expansion, but it's too noisy
    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, flee)) should be (successful)
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

    val tester = new GraphTester(text)

    // Sentence 1

    // Sentence 2
    val rains1 = NodeSpec("Seasonal rains", Quant("above-average"), Inc("above-average"))

    // Sentence 3

    // Sentence 4
    val conditions = NodeSpec("Weather conditions") // todo: we should handle sentiment (i.e., 'favorable')
    val rains3 = NodeSpec("seasonal rains", Quant("average to above average"))
    val benefiting = NodeSpec("vegetation conditions", Inc("benefiting"))

    behavior of "TestDoc4 Paragraph 4"

    passingTest should "have correct node 1" taggedAs(Somebody) in {
      tester.test(rains1) should be (successful)
    }
    passingTest should "have correct node 4" taggedAs(Somebody) in {
      tester.test(conditions) should be (successful)
    }
    // todo: I want to add positively affect and negatively affect to the
    futureWorkTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rains3, Causal, benefiting)) should be (successful)
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

    val tester = new GraphTester(text)

    // Sentence 1
    val prospects = NodeSpec("prospects for 2017 aggregate cereal production", Quant("unfavourable", "generally"))
    val insecurity = NodeSpec("protracted and widespread insecurity", Inc("widespread"))
    val access = NodeSpec("farmers' access to fields", Dec("constraining"))
    val displacement = NodeSpec("large scale displacement of people", Quant("large"))
    val shortages = NodeSpec("input shortages", Dec("shortages"))
    val damage = NodeSpec("households' productive assets", Dec("damage"))

    // Sentence 2
    val production = NodeSpec("crop production is expected to be lower than the already poor 2016 output", Dec("lower"), TimEx("2016"), Quant("poor"), Dec("poor"))
    val displacements = NodeSpec("recent massive displacements", Quant("massive"), TimEx("recent"))

    // Sentence 3
    val population = NodeSpec("population of the former Central Equatoria State", Quant("75 percent"))

    behavior of "TestDoc4 Paragraph 5"

    futureWorkTest should "have correct nodes 1" taggedAs(Somebody) in {
      tester.test(population) should be (successful)
    }
    // there isn't really enough of a link here - trigger would be "as" !
    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, prospects)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, access)) should be (successful)
    }
    passingTest should "have correct edges 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, displacement)) should be (successful)
    }
    passingTest should "have correct edges 4" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, shortages)) should be (successful)
    }
    passingTest should "have correct edges 5" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, damage)) should be (successful)
    }
    passingTest should "have correct edges 6" taggedAs(Somebody) in {
      tester.test(EdgeSpec(displacements, Causal, production)) should be (successful)
    }
}

  { // Paragraph 6
    val text = """
In addition, Fall Armyworm infestations have been reported in all
regions of the country, with significant crop damage, especially in
parts of former Northern Bahr el Ghazal, Eastern Equatoria and
Central Equatoria states.
      """

    val tester = new GraphTester(text)

    val infestations = NodeSpec("Fall Armyworm infestations")
    val damage = NodeSpec("crop damage", Quant("significant"))

    behavior of "TestDoc4 Paragraph 6"

    // we removed the "with" rules bc they weren't precise enough
    futureWorkTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(infestations, Correlation, damage)) should be (successful)
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

    val tester = new GraphTester(text)

    // Sentence 1
    val prices = NodeSpec("prices of maize and sorghum", Inc("doubled", "more than"))
    val pricesTo = NodeSpec("prices of maize and sorghum")
    val situation = NodeSpec("tight supply situation")
    val disruptions = NodeSpec("market disruptions")
    val hyperinflation = NodeSpec("hyperinflation")
    val depreciation = NodeSpec("depreciation of the local currency", Dec("significant"))

    // Sentence 2
    val they = NodeSpec("they", Dec("declined", "by about 12 percent")) // coreference?
    val harvest = NodeSpec("first season harvest")
    val selling = NodeSpec("selling at subsidized prices")

    // Sentence 3
//    val prices2 = NodeSpec("Prices of groundnuts", Dec("decreased", "by 22 percent"))
    val prices2 = NodeSpec("Prices of groundnuts", Dec("decreased"), TimEx("the same period"))
    val prices3 = NodeSpec("prices of wheat flour", Quant("soar"))
    val prices3To = NodeSpec("prices of wheat flour", Quant("new record highs"))

    // Sentence 4
    val prices4 = NodeSpec("Overall, prices of these food staples in August were more than twice the high levels in August last year", Inc("high"), Quant("high"), TimEx("August last year"))
    val prices5 = NodeSpec("prices of these food staples", Quant("12 times higher"))

    behavior of "TestDoc4 Paragraph 7"

    passingTest should "have correct node 1" taggedAs(Somebody) in {
      tester.test(pricesTo) should be (successful)
    }
    failingTest should "have correct node 2" taggedAs(Somebody) in {
      tester.test(situation) should be (successful)
    }
    passingTest should "have correct node 3" taggedAs(Somebody) in {
      tester.test(prices2) should be (successful)
    }
    futureWorkTest should "have correct node 4" taggedAs(Somebody) in {
      tester.test(prices3) should be (successful)
    }
    futureWorkTest should "have correct node 5" taggedAs(Somebody) in {
      tester.test(prices3To) should be (successful)
    }
    passingTest should "have correct node 6" taggedAs(Somebody) in {
      tester.test(prices4) should be (successful)
    }
    futureWorkTest should "have correct node 7" taggedAs(Somebody) in {
      tester.test(prices5) should be (successful)
    }

    passingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(situation, Causal, prices)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(disruptions, Causal, prices)) should be (successful)
    }
    passingTest should "have correct edges 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(hyperinflation, Causal, prices)) should be (successful)
    }
    passingTest should "have correct edges 4" taggedAs(Somebody) in {
      tester.test(EdgeSpec(depreciation, Causal, prices)) should be (successful)
    }
    passingTest should "have correct edges 5" taggedAs(Somebody) in {
      tester.test(EdgeSpec(harvest, Correlation, they)) should be (successful)
    }
    passingTest should "have correct edges 6" taggedAs(Somebody) in {
      tester.test(EdgeSpec(selling, Correlation, they)) should be (successful)
    }
  }
}
