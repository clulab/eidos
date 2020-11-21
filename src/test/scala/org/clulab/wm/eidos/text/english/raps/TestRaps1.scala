package org.clulab.wm.eidos.text.english.raps

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.TestUtils._


class TestRaps1 extends EnglishTest {

  { //3 Increase
    val sent20 = "Hence, government liberalizes imports of food grains, invests in food chain logistics, and boost research and development for new crop cultivars to boost agricultural production for ensuring food security."
    val tester = new GraphTester(sent20)

    val research = NodeSpec("research", Inc("boost"))
    val dev = NodeSpec("development", Inc("boost"))
    val prod = NodeSpec("agricultural production", Inc("boost"))


    behavior of "Raps_sent20"

    passingTest should "have the correct node 1" taggedAs(Heather) in {
      tester.test(research) should be (successful)
    }

    passingTest should "have the correct node 2" taggedAs(Heather) in {
      tester.test(dev) should be (successful)
    }

    passingTest should "have the correct node 3" taggedAs(Heather) in {
      tester.test(prod) should be (successful)
    }

  }

  {
    //Unsure exactly what this sentences convey. Are there actually any causal events here?
    val sent21 = "A combination of increasing population, government plans to invest in fertilizer factory, " +
      "government subsidy on fertilizers, improved economic performance expected to cause a shift from agriculture to service industry, " +
      "government plans for massive expansion of irrigation (irrigate 1 million ha.), newly devolved county governments etc. are some of the developments " +
      "expected to change agriculture development in the country."
    val tester = new GraphTester(sent21)

    val population = NodeSpec("combination of increasing population", Inc("increasing"))
    val econ = NodeSpec("economic performance", Inc("improved"))

    behavior of "Raps_sent21"

    // Not expanding singleton nodes anymore...
//    passingTest should "have correct node 1" taggedAs(Heather) in {
//      tester.test(population) should be (successful)
//    }
//
//    //currently extracts "economic performance expected" as the entity text
//    failingTest should "have correct node 2" taggedAs(Heather) in {
//      tester.test(econ) should be (successful)
//    }

  }

  {
    val sent22 = "Along with the support programs such as agricultural insurance and input subsidies, the government efforts and investments will be increased for extending irrigation services, agricultural mechanization, and developing disaster risk-management practices."
    val tester = new GraphTester(sent22)

    val gov = NodeSpec("government efforts", Inc("increased"))
    val investments = NodeSpec("investments", Inc("increased"))

    behavior of "Raps_sent22"

    passingTest should "have correct node 1" taggedAs(Heather) in {
      tester.test(gov) should be (successful)
    }

    passingTest should "have correct node 2" taggedAs(Heather) in {
      tester.test(investments) should be (successful)
    }

  }

  {
    val sent23 = "The transformation however starts under extremely difficult conditions, characterized by large account deficit and liquidity challenges and limited direct foreign investment due to lack of clarity on investment security and high interest rates."
    val tester = new GraphTester(sent23)

    val account = NodeSpec("large account deficit", Dec("deficit", "large"), Quant("large"))
    val invest = NodeSpec("limited direct foreign investment", Dec("limited"))
    val noClarity = NodeSpec("clarity", Dec("lack"))
    val interestRates = NodeSpec("interest rates", Quant("high"), Inc("high"))

    val conditions = NodeSpec("extremely difficult conditions", Dec("difficult", "extremely"))

    behavior of "Raps_sent23"

    passingTest should "have correct node 1" taggedAs(Heather) in {
      tester.test(conditions) should be (successful)
    }

    //extracts properly in the Eidos shell
    passingTest should "have correct node 2" taggedAs(Heather) in {
      tester.test(account) should be (successful)
    }

    failingTest should "have correct edge 1" taggedAs(Heather) in {
      tester.test(EdgeSpec(noClarity, Causal, invest)) should be (successful)
    }

    failingTest should "have correct edge 2" taggedAs(Heather) in {
      tester.test(EdgeSpec(interestRates, Causal, invest)) should be (successful)
    }

  }

  { //TODO: does "adversely" affect mean a decrease?
    val sent24 = "Global trends suggest that rice wheat production in the region will be adversely affected by climate change."
    val tester = new GraphTester(sent24)

    val wheat = NodeSpec("rice wheat production in the region")
    val climate = NodeSpec("climate")

    behavior of "Raps_24"

    affectEventTest should "have correct edge" taggedAs(Heather) in {
      tester.test(EdgeSpec(climate, Affect, wheat)) should be (successful)
    }

  }

  {
    val sent25 = "With a high cost of production and degraded natural resources, profitability in agriculture may be further reduced, making agriculture unprofitable."
    val tester = new GraphTester(sent25)

    val cost = NodeSpec("cost of production", Quant("high"), Inc("high"))
    val resources = NodeSpec("natural resources", Dec("degraded"))
    val profit = NodeSpec("profitability in agriculture", Dec("reduced"))


    behavior of "Raps_25"

    failingTest should "have correct edge 1" taggedAs(Heather) in {
      tester.test(EdgeSpec(cost, Causal, profit)) should be (successful)

    }

    failingTest should "have correct edge 2" taggedAs(Heather) in {
      tester.test(EdgeSpec(resources, Causal, profit)) should be (successful)

    }

  }

  {
    val sent26 = "Labor migration to urban areas, non agricultural activities and impact of HIV/AIDS also leads to labor shortages."
    val tester = new GraphTester(sent26)

    val migration = NodeSpec("Labor migration to urban areas")
    val activities = NodeSpec("non agricultural activities")
    val hiv = NodeSpec("impact of HIV/AIDS")
    val shortage = NodeSpec("labor", Dec("shortages"))

    behavior of "Raps_26"

    failingTest should "have correct edge 1" taggedAs(Heather) in {
      tester.test(EdgeSpec(migration, Causal, shortage)) should be (successful)
    }

    failingTest should "have correct edge 2" taggedAs(Heather) in {
      tester.test(EdgeSpec(activities, Causal, shortage)) should be (successful)
    }

    failingTest should "have correct edge 3" taggedAs(Heather) in {
      tester.test(EdgeSpec(hiv, Causal, shortage)) should be (successful)
    }
  }

  {
    val sent27 = "Agricultural production and profitability are declining, land is degrading and being underutilized."
    val tester = new GraphTester(sent27)

    val prod = NodeSpec("Agricultural production", Dec("declining"))
    val profits = NodeSpec("profitability", Dec("declining"))
    val land = NodeSpec("land", Dec("degrading"))

    behavior of "Raps_27"

    passingTest should "have correct node 1" taggedAs(Heather) in {
      tester.test(prod) should be (successful)
    }

    passingTest should "have correct node 2" taggedAs(Heather) in {
      tester.test(profits) should be (successful)
    }

    failingTest should "have correct node 3" taggedAs(Heather) in {
      tester.test(land) should be (successful)
    }

  }

  {
    val sent28 = "Soil quality will decline by a small-to-medium extent, due to pollution, and intensive cultivation will be caused by a shrinking land base for agriculture."
    val tester = new GraphTester(sent28)

    val soil = NodeSpec("Soil quality", Dec("decline", "small-to-medium"))
    val pollution = NodeSpec("pollution")
    val shrink = NodeSpec("shrinking land base for agriculture", Dec("shrinking"))
    val cultivation = NodeSpec("intensive cultivation", Inc("intensive"))

    behavior of "Raps_28"

    failingTest should "have correct edge 1" taggedAs(Heather) in {
      tester.test(EdgeSpec(pollution, Causal, soil)) should be (successful)
    }

    passingTest should "have correct edge 2" taggedAs(Heather) in {
      tester.test(EdgeSpec(shrink, Causal, cultivation)) should be (successful)
    }

  }

  {
    val sent29 = "Labor migration and HIV/AIDS result in labor shortage."
    val tester = new GraphTester(sent29)

    val migration = NodeSpec("Labor migration")
    val hiv = NodeSpec("HIV/AIDS")
    val labor = NodeSpec("labor", Dec("shortage"))

    behavior of "Raps_29"

    failingTest should "have correct edge 1" taggedAs(Heather) in {
      tester.test(EdgeSpec(migration, Causal, labor)) should be (successful)
    }

    failingTest should "have correct edge 2" taggedAs(Heather) in {
      tester.test(EdgeSpec(hiv, Causal, labor)) should be (successful)
    }

  }

  {
    val sent30 = "Share of agriculture in overall economy will decrease with increase in inequality."
    val tester = new GraphTester(sent30)

    val share = NodeSpec("Share of agriculture in overall economy", Dec("decrease"))
    val inequality = NodeSpec("inequality", Inc("increase"))

    behavior of "Raps_30"

    failingTest should "have correct edge" taggedAs(Heather) in {
      tester.test(EdgeSpec(inequality, Causal, share)) should be (successful)
    }

  }

  {
    val sent31 = "The adoption process will be instigated due to the anticipated losses in agricultural productivity in the face of climatic uncertainties."
    val tester = new GraphTester(sent31)

    val adoption = NodeSpec("adoption process")
    val losses = NodeSpec("anticipated losses")
    val prod = NodeSpec("agricultural productivity", Dec("losses"))

    behavior of "Raps_31"

    failingTest should "have correct edge" taggedAs(Heather) in {
      tester.test(EdgeSpec(losses, Causal, adoption)) should be (successful)
    }

  }


}
