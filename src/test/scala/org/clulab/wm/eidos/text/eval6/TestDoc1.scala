package org.clulab.wm.eidos.text.eval6

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text._

class TestDoc1 extends Test {
  // 130035 excerpt BG.pdf (BECKY)
  { // Paragraph 1
    val text = """
      |Trade with Sudan, South Sudan's second most important trading partner, has decreased significantly
      |since independence. Trade between the two countries now is mainly informal.
      |However, in 2012 the border was closed and supply was cut off (World Bank 2014).
      |Since 2013 some borders have been reopened, but trade has been slow to recover.
      """

    val tester = new Tester(text)

    val trade1 = NodeSpec("Trade with Sudan", Dec("decreased", "significantly"))
    val trade2 = NodeSpec("Trade", Unmarked("mainly informal")) // todo: expand Unmarked() to handle quantifiers?
    val border = NodeSpec("border", Unmarked("closed"))
    val supply = NodeSpec("supply", Dec("cut"))
    val borders = NodeSpec("borders", Unmarked("reopened"))  // todo: ??
    val trade3 = NodeSpec("trade", Unmarked("slow to recover")) // todo

    behavior of "TestDoc1 Paragraph 1"

    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(trade1)
    }
    futureWorkTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(trade2)
    }
    futureWorkTest should "have correct edge 1" taggedAs (Somebody) in {
      tester.test(EdgeSpec(border, Correlation, supply)) should be (successful)
    }
    futureWorkTest should "have correct singleton node 3" taggedAs(Somebody) in {
      tester.test(borders)
    }
    futureWorkTest should "have correct singleton node 4" taggedAs(Somebody) in {
      tester.test(trade3)
    }
  }

  { // Paragraph 2
    val text = """
       |Currently, most crop cultivation is done on small farms, producing little if
       |any marketable surplus.
       """

    val tester = new Tester(text)

    val futureWorkCrop = NodeSpec("crop cultivation", Unmarked("done on small farms"))
    val crop = NodeSpec("crop cultivation")
    val surplus = NodeSpec("marketable surplus", Quant("little if any"))

    behavior of "TestDoc1 Paragraph 2"

    failingTest should "have correct edge 1" taggedAs (Becky) in {
      tester.test(EdgeSpec(crop, Causal, surplus)) should be (successful)
    }

  }

  { // Paragraph 3 (highlighted)
    val text = """
     |Rainfall in the Hills and Mountains region of the northern half of Central Equatoria
     |and the western half of Eastern Equatoria (500 to 800 mm per year) is also sufficient
     |to support substantial crop agriculture (WFP 2011).
     """

    val tester = new Tester(text)

    val rainfall = NodeSpec("Rainfall in the Hills and Mountains region of the northern half of Central Equatoria",
      Quant("sufficient"))
    val agriculture = NodeSpec("crop agriculture")

    behavior of "TestDoc1 Paragraph 3"

    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall, Causal, agriculture))
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
     |services -- all factors that could lead to further soil nutrient depletion (Kowr 2013).
     """

    val tester = new Tester(text)

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
    val rainfall = NodeSpec("rainfall", Quant("heavy"))
    val fertiizerUse = NodeSpec("fertilizer use", Dec("low"))
    val overfarming = NodeSpec("over-farming")
    val knowledge = NodeSpec("knowledge", Dec("limited"))
    val roleOfFertilizer = NodeSpec("understanding of the role of fertilizer in improved crop production", Dec("lack"))
    val extension = NodeSpec("access to extension", Dec("have")) // with negation, also todo: I think access is transparent
    val transfer = NodeSpec("transfer services", Dec("have"))
    val soilNutient2 = NodeSpec("soil nutrient", Dec("depletion"))


    behavior of "TestDoc1 Paragraph 4"

    futureWorkTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(soilPhosphorous)
    }
    futureWorkTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(soilOrganicMatter)
    }
    futureWorkTest should "have correct singleton node 3" taggedAs(Somebody) in {
      tester.test(soils)
    }
    futureWorkTest should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(leaching, Causal, soilNutrient))
    }
    passingTest should "have correct edge 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall, Correlation, leaching))
    }
    passingTest should "have correct edge 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(fertiizerUse, Causal, soilNutrient))
    }
    passingTest should "have correct edge 4" taggedAs(Somebody) in {
      tester.test(EdgeSpec(overfarming, Causal, soilNutrient))
    }
    passingTest should "have correct edge 5" taggedAs(Somebody) in {
      tester.test(EdgeSpec(knowledge, Causal, soilNutient2))
    }
    passingTest should "have correct edge 6" taggedAs(Somebody) in {
      tester.test(EdgeSpec(roleOfFertilizer, Causal, soilNutient2))
    }
    passingTest should "have correct edge 7" taggedAs(Somebody) in {
      tester.test(EdgeSpec(extension, Causal, soilNutient2))
    }
    passingTest should "have correct edge 8" taggedAs(Somebody) in {
      tester.test(EdgeSpec(transfer, Causal, soilNutient2))
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

    val tester = new Tester(text)

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
    val publicInvestment = NodeSpec("public investment in recent decades", Dec("low", "extremely"))
    val irrigation = NodeSpec("irrigation infrastructure", Dec("exists")) // with Neg of "no"
    val roads = NodeSpec("roads", Unmarked("poorly maintained"), Unmarked("not repaired"), Unmarked("washed out")) // fixme - dec??
    val transportation = NodeSpec("transportation infrastructure", Dec("inadequate"), Quant("inadequate"))
    // todo: resolve the Unmarked ones...
    val transportFutureWork = NodeSpec("subsistence farmers to transport surpluses to markets", Unmarked("difficult"), Quant("expensive"))
    // todo/fixme : in this domain, does difficult == decrease??
    val transport = NodeSpec("subsistence farmers to transport surpluses to markets", Dec("difficult"), Quant("expensive"))

    behavior of "TestDoc1 Paragraph 5"

    passingTest should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(institutional, Causal, development))
    }
    passingTest should "have correct edge 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(disagreements, Causal, conflict))
    }
    passingTest should "have correct edge 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(livestock, Causal, conflict))
    }
    passingTest should "have correct edge 4" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, production))
    }
    passingTest should "have correct edge 5" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, building))
    }
    passingTest should "have correct edge 5b" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, building2))
    }
    passingTest should "have correct edge 6" taggedAs(Somebody) in {
      tester.test(EdgeSpec(publicInvestment, Correlation, irrigation))
    }
    futureWorkTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(roads)
    }
    passingTest should "have correct edge 7" taggedAs(Somebody) in {
      tester.test(EdgeSpec(transportation, Causal, transport))
    }
  }

  { // Paragraph 6
    val text = """
     |Further, poor business practices and a lack of information about market prices make it difficult
     |for businesses to develop along the agriculture value chain.
     """

    val tester = new Tester(text)

    val business = NodeSpec("business practices", Dec("poor"), Quant("poor")) // or Unmarked("sufficient to support ...)
    val information = NodeSpec("information about market prices", Dec("lack"))
    // todo/fixme : in this domain, does difficult == decrease??
    val develop = NodeSpec("businesses to develop", Dec("difficult"))
    behavior of "TestDoc1 Paragraph 6"

    passingTest should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(business, Causal, develop))
    }
    passingTest should "have correct edge 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(information, Causal, develop))
    }

  }
}
