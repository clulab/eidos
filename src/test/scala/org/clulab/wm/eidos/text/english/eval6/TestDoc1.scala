package org.clulab.wm.eidos.text.english.eval6

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.TestUtils._

class TestDoc1 extends EnglishTest {
  // 130035 excerpt BG.pdf (BECKY)
  { // Paragraph 1
    val text = """
      |Trade with Sudan, South Sudan's second most important trading partner, has decreased significantly
      |since independence. Trade between the two countries now is mainly informal.
      |However, in 2012 the border was closed and supply was cut off (World Bank 2014).
      |Since 2013 some borders have been reopened, but trade has been slow to recover.
      """

    val tester = new GraphTester(text)

    val trade1 = NodeSpec("Trade", Dec("decreased", "significantly"))
    val trade2 = NodeSpec("Trade", Unmarked("mainly informal")) // todo: expand Unmarked() to handle quantifiers?
    val border = NodeSpec("border", Unmarked("closed"))
    val supply = NodeSpec("supply", Dec("cut"))
    val borders = NodeSpec("borders", Unmarked("reopened"))  // todo: ??
    val trade3 = NodeSpec("trade", Unmarked("slow to recover")) // todo

    behavior of "TestDoc1 Paragraph 1"

    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(trade1) should be (successful)
    }
    futureWorkTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(trade2) should be (successful)
    }
    futureWorkTest should "have correct edge 1" taggedAs (Somebody) in {
      tester.test(EdgeSpec(border, Correlation, supply)) should be (successful)
    }
    futureWorkTest should "have correct singleton node 3" taggedAs(Somebody) in {
      tester.test(borders) should be (successful)
    }
    futureWorkTest should "have correct singleton node 4" taggedAs(Somebody) in {
      tester.test(trade3) should be (successful)
    }
  }

  { // Paragraph 2
    val text = """
       |Currently, most crop cultivation is done on small farms, producing little if
       |any marketable surplus.
       """

    val tester = new GraphTester(text)

    val futureWorkCrop = NodeSpec("crop cultivation", Unmarked("done on small farms"))
    val crop = NodeSpec("most crop cultivation", Quant("most"))
    val surplus = NodeSpec("marketable surplus", Dec("little if any"))

    behavior of "TestDoc1 Paragraph 2"

    passingTest should "have correct edge 1" taggedAs (Becky) in {
      tester.test(EdgeSpec(crop, Causal, surplus)) should be (successful)
    }

  }

  { // Paragraph 3 (highlighted)
    val text = """
     |Rainfall in the Hills and Mountains region of the northern half of Central Equatoria
     |and the western half of Eastern Equatoria (500 to 800 mm per year) is also sufficient
     |to support substantial crop agriculture (WFP 2011).
     """

    val tester = new GraphTester(text)

    val rainfall = NodeSpec("Rainfall in the Hills and Mountains region of the northern half of Central Equatoria",
      Quant("sufficient"))
    val agriculture = NodeSpec("crop agriculture")

    behavior of "TestDoc1 Paragraph 3"

    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      // removing bc not expanding non-causal nodes
//      tester.test(EdgeSpec(rainfall, Causal, agriculture)) should be (successful)
    }

  }

  { // Paragraph 4
    val text = """
     |In most areas of South Sudan, soils are low in phosphorous as well as in organic matter.
     |Despite this, most soils are moderately fertile. But soils could quickly become nutrient
     |deficient due to leaching related to heavy rainfall, low levels of fertilizer use (on
     |average only 4 kg per ha of fertilizers are used in South Sudan), and over-farming.
     |Smallholders have limited knowledge and capacity, lack an understanding of the role of
     |fertilizer in improved crop production, and do not have access to extension or transfer
     |services--all factors that could lead to further soil nutrient depletion (Kowr 2013).
     """

    val tester = new GraphTester(text)

    // TODO: for now we are tabling this conversation, but we will need to decide on representations at some point
    // we will perhaps handle them with canonicalForm
    val soilPhosphorous = NodeSpec("phosphorous in soils", Dec("low")) // todo: can't do this with curr spans
    val soilOrganicMatter = NodeSpec("organic matter in soils", Dec("low")) // todo: can't do this with curr spans
    // We could maybe resolve something with the "Despite..."
    val soils = NodeSpec("soils", Unmarked("moderately fertile"))
    // ----- todo: Which of these?
    val soils2 = NodeSpec("soils", Unmarked("nutrient deficient"))
    val soilNutrient = NodeSpec("nutrient in soil", Dec("deficient"))
    // -----
    val leaching = NodeSpec("leaching")
    val rainfall = NodeSpec("heavy rainfall", Quant("heavy"), Inc("heavy"))
    val fertilizerUse = NodeSpec("low levels of fertilizer use (on average only 4 kg per ha of fertilizers are used in South Sudan)", Dec("low"))
    val overfarming = NodeSpec("over-farming")
    val knowledge = NodeSpec("knowledge", Dec("limited"))
    val roleOfFertilizer = NodeSpec("understanding of the role of fertilizer in improved crop production", Dec("lack"))
    val extension = NodeSpec("access to extension", Dec("have")) // with negation, also todo: I think access is transparent
    val transfer = NodeSpec("transfer services", Dec("have"))
    val soilNutient2 = NodeSpec("soil nutrient", Dec("depletion"))


    behavior of "TestDoc1 Paragraph 4"

    futureWorkTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(soilPhosphorous) should be (successful)
    }
    futureWorkTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(soilOrganicMatter) should be (successful)
    }
    futureWorkTest should "have correct singleton node 3" taggedAs(Somebody) in {
      tester.test(soils) should be (successful)
    }
    futureWorkTest should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(leaching, Causal, soilNutrient)) should be (successful)
    }
    passingTest should "have correct edge 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall, Correlation, leaching)) should be (successful)
    }
    brokenSyntaxTest should "have correct edge 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(fertilizerUse, Causal, soilNutrient)) should be (successful)
    }
    brokenSyntaxTest should "have correct edge 4" taggedAs(Somebody) in {
      tester.test(EdgeSpec(overfarming, Causal, soilNutrient)) should be (successful)
    }
    passingTest should "have correct edge 5" taggedAs(Somebody) in {
      tester.test(EdgeSpec(knowledge, Causal, soilNutient2)) should be (successful)
    }
    passingTest should "have correct edge 6" taggedAs(Somebody) in {
      tester.test(EdgeSpec(roleOfFertilizer, Causal, soilNutient2)) should be (successful)
    }
    passingTest should "have correct edge 7" taggedAs(Somebody) in {
      tester.test(EdgeSpec(extension, Causal, soilNutient2)) should be (successful)
    }
    passingTest should "have correct edge 8" taggedAs(Somebody) in {
      tester.test(EdgeSpec(transfer, Causal, soilNutient2)) should be (successful)
    }
  }

  { // Paragraph 5 (highlighted)
    val text = """
     |Institutional weakness has further hindered development of the agriculture sector.
     |Disagreements over land rights for crop cultivation and livestock grazing continue to be a major source of conflict.
     |This insecurity discourages farmers from expanding production, and traders and retailers from building
     |marketing infrastructure. The extremely low level of public investment in recent decades has meant that
     |essentially no irrigation infrastructure exists and only 2 percent of South Sudan's limited road network is
     |paved. Roads are poorly maintained, not repaired, and completely washed out during the rainy season
     |(World Bank 2012; USAID and Fintrac 2012). Because of this inadequate transportation infrastructure, it
     |is difficult and expensive for subsistence farmers to transport surpluses to markets.
     """

    val tester = new GraphTester(text)

    val institutional = NodeSpec("Institutional", Dec("weakness"))
    val development = NodeSpec("development of the agriculture sector", Dec("hindered"))
    val disagreements = NodeSpec("Disagreements over land rights for crop cultivation")
    val livestock = NodeSpec("livestock grazing") // likely should be disagreements over... too, but PP attachment is ambiguous
    val conflict = NodeSpec("conflict")
    val insecurity = NodeSpec("insecurity") // todo: resolve coref "this insecurity"
    val production = NodeSpec("farmers from expanding production", Dec("discourages"))
    // todo: These next two should be handled better at some point
    val building = NodeSpec("traders", Dec("discourages"))
    val building2 = NodeSpec("retailers from building marketing infrastructure", Dec("discourages"))
    val publicInvestment = NodeSpec("extremely low level of public investment in recent decades", Dec("low", "extremely"), Quant("low", "extremely"), TimEx("recent decades"))
    val irrigation = NodeSpec("essentially no irrigation infrastructure exists", Dec("no")) // with Neg of "no"
    val paved = NodeSpec("only 2 percent of South Sudan's limited road network is paved", Dec("limited"), GeoLoc("South Sudan"))
    val roads = NodeSpec("roads", Unmarked("poorly maintained"), Unmarked("not repaired"), Unmarked("washed out")) // fixme - dec??
    val transportation = NodeSpec("inadequate transportation infrastructure", Dec("inadequate"), Quant("inadequate"))
    // todo: resolve the Unmarked ones...
    val transport = NodeSpec("subsistence farmers to transport surpluses to markets", Dec("difficult")) // ideally "expensive" too

    behavior of "TestDoc1 Paragraph 5"

    passingTest should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(institutional, Causal, development)) should be (successful)
    }
    passingTest should "have correct edge 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(disagreements, Causal, conflict)) should be (successful)
    }
    passingTest should "have correct edge 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(livestock, Causal, conflict)) should be (successful)
    }
    passingTest should "have correct edge 4" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, production)) should be (successful)
    }
    passingTest should "have correct edge 5" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, building)) should be (successful)
    }
    passingTest should "have correct edge 5b" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, building2)) should be (successful)
    }
    passingTest should "have correct edge 6" taggedAs(Somebody) in {
      tester.test(EdgeSpec(publicInvestment, Correlation, irrigation)) should be (successful)
    }
    passingTest should "have correct edge 6b" taggedAs(Somebody) in {
      tester.test(EdgeSpec(publicInvestment, Correlation, paved)) should be (successful)
    }
    futureWorkTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(roads) should be (successful)
    }
    passingTest should "have correct edge 7" taggedAs(Somebody) in {
      tester.test(EdgeSpec(transportation, Causal, transport)) should be (successful)
    }
  }

  { // Paragraph 6
    val text = """
     |Further, poor business practices and a lack of information about market prices make it difficult
     |for businesses to develop along the agriculture value chain.
     """

    val tester = new GraphTester(text)

    val business = NodeSpec("business practices", Dec("poor"), Quant("poor")) // or Unmarked("sufficient to support ...)
    val information = NodeSpec("information about market prices", Dec("lack"))
    // todo/fixme : in this domain, does difficult == decrease??
    val develop = NodeSpec("businesses to develop", Dec("difficult"))
    behavior of "TestDoc1 Paragraph 6"

    futureWorkTest should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(business, Causal, develop)) should be (successful)
    }
    futureWorkTest should "have correct edge 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(information, Causal, develop)) should be (successful)
    }

  }
}
