package org.clulab.wm.eidos.text.english.raps

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.EnglishTest
import org.clulab.wm.eidos.test.TestUtils._

class TestRaps extends EnglishTest {

  { //One Increase attachments
    val sent1 = "Better and well-functioning agricultural credit and market services for both established and emerging farmers."
    val tester = new GraphTester(sent1)

    val credit = NodeSpec("well-functioning agricultural credit", Inc("Better") )

    behavior of "Raps_sent1"

    tempBrokenEntitiesTest should "have the correct node" taggedAs(Heather) in {
      tester.test(credit) should be (successful)

    }

  }

  { //3 Increase attachments
    val sent2 = "The support for agricultural research, education, and extension programs will also be increased for developing and disseminating climate change adaptation agricultural technologies to the farmers."
    // Note: parse of sentence makes it "impossible" to extract increase for education and extension programs
    // Maybe a reason to switch to cluprocessor
    val tester = new GraphTester(sent2)

    //ALL of these fail to be increase events
    val research = NodeSpec("agricultural research", Inc("increased"))
    val education = NodeSpec("education", Inc("increased"))
    val programs = NodeSpec("extension programs", Inc("increased"))

    behavior of "Raps_sent2"

    failingTest should "have correct nodes 1" taggedAs(Heather) in {
      tester.test(research) should be (successful)
    }

    failingTest should "have correct nodes 2" taggedAs(Heather) in {
      tester.test(education) should be (successful)
    }

    failingTest should "have correct nodes 3" taggedAs(Heather) in {
      tester.test(programs) should be (successful)
    }

  }

  { //3 Decrease, 2 Increase Attachments; 2 Causal Edges, 1 Origin Edge
    val sent3 = "Limited financial capacities and low education levels further restrict farmers' ability for higher benefits from increased agricultural production."
    val tester = new GraphTester(sent3)

    val financial = NodeSpec("Limited financial capacities", Dec("Limited"))
    val ability = NodeSpec("farmers' ability for higher benefits from increased agricultural production", Dec("restrict"), Inc("higher"), Inc("increased"))
    val education = NodeSpec("low education levels", Dec("low"), Quant("low"))

//    val production = NodeSpec("agricultural production", Inc("increased"))
//    val benefits = NodeSpec("benefits", Inc("higher"))

    behavior of "Raps_sent3"

    passingTest should "have correct edges 1" taggedAs(Heather) in {
      tester.test(EdgeSpec(financial, Causal, ability)) should be (successful)
    }

    passingTest should "have correct edges 2" taggedAs(Heather) in {
      tester.test(EdgeSpec(education, Causal, ability)) should be (successful)
    }

    //Origin EventSpec Test
//    futureWorkTest should "have correct edges 3" taggedAs(Heather) in {
//      tester.test(EdgeSpec(production, Origin, benefits)) should be (successful)
//    }

  }

  { //3 Increase, 7 Decrease Attachments, 10 Causal
    val sent4 = "The government promotes improved cultivar and climate-smart technologies but the policy to cut down the use of inorganic fertilizer " +
      "and phase out the fertilizer subsidy results in deteriorating biophysical conditions, low use of inorganic fertilizer, less water, reduced farm sizes which lead to low benefit from the improved cultivar."

    val tester = new GraphTester(sent4)
    //increase
    val cultivar1 = NodeSpec("improved cultivar", Pos("improved"), Inc("promotes"))
    val tech = NodeSpec("climate-smart technologies", Pos("improved"), Inc("promotes"))

    //decrease
    val fertUse1 = NodeSpec("use of inorganic fertilizer", Dec("cut"))
    val fertUse2 = NodeSpec("use of inorganic fertilizer", Dec("low"), Quant("low"))
    val subsidy = NodeSpec("fertilizer subsidy", Dec("phase out"))
    val conditions = NodeSpec("biophysical conditions", Dec("deteriorating"))
    val water = NodeSpec("water", Dec("less"))
    val farmSize = NodeSpec("farm sizes", Dec("reduced"))
    val benefit = NodeSpec("benefit", Dec("low"), Quant("low"))
    //other
    val gov = NodeSpec("government")
    val policy = NodeSpec("policy")

    behavior of "Raps_sent4"

    //The govt promotes improved cultivar
    passingTest should "have correct edges 1" taggedAs(Heather) in {
      tester.test(EdgeSpec(gov, Causal, cultivar1)) should be (successful)
    }

    //The govt promotes improved... climate-smart technologies
    passingTest should "have correct edges 2" taggedAs(Heather) in {
      tester.test(EdgeSpec(gov, Causal, tech)) should be (successful)
    }

    //the policy ... results in deteriorating biophysical conditions
    //unsure why policy node fails, as node appears in Eidos shell
    futureWorkTest should "have correct edges 3" taggedAs(Heather) in {
      //unsure why policy node fails, as node appears in Eidos shell
      //tester.test(policy) should be (successful)//fails, although policy is an entity in Eidos shell
      //tester.test(conditions) should be (successful)//passes
      tester.test(EdgeSpec(policy, Causal, conditions)) should be (successful)
    }

    //phase out the fertilizer subsidy results in deteriorating biophysical conditions
    tempBrokenEntitiesTest should "have correct edges 4" taggedAs(Heather) in {
      tester.test(EdgeSpec(subsidy, Causal, conditions)) should be (successful)
    }

    //phase out subsidy results in ... low use of inorganic fertilizer
    tempBrokenEntitiesTest should "have correct edges 5" taggedAs(Heather) in {
      tester.test(EdgeSpec(subsidy, Causal, fertUse2)) should be (successful)
    }

    //phase out results in ... less water
    tempBrokenEntitiesTest should "have correct edges 6" taggedAs(Heather) in {
      tester.test(EdgeSpec(subsidy, Causal, water)) should be (successful)
    }

    //phase out results in ... reduced farm size
    tempBrokenEntitiesTest should "have correct edges 7" taggedAs(Heather) in {
      tester.test(EdgeSpec(subsidy, Causal, farmSize)) should be (successful)
    }

    //low use of inorganic fertilizer... which lead to low benefit
    tempBrokenEntitiesTest should "have correct edges 8" taggedAs(Heather) in {
      tester.test(EdgeSpec(fertUse2, Causal, benefit)) should be (successful)
    }

    //less water ... lead to low benefit
    tempBrokenEntitiesTest should "have correct edges 9" taggedAs(Heather) in {
      tester.test(EdgeSpec(water, Causal, benefit)) should be (successful)
    }

    //reduced farm sizes which lead to low benefit from the improved cultivar
    tempBrokenEntitiesTest should "have correct edges 10" taggedAs(Heather) in {
      tester.test(EdgeSpec(farmSize, Causal, benefit)) should be (successful)
    }

  }

  {//1 Increase
    val sent5 = "With increases in poverty levels people become more vulnerable to climate change and other risks."
    val tester = new GraphTester(sent5)

    val poverty = NodeSpec("poverty levels", Inc("increases"))

    behavior of "Raps_sent5"

    passingTest should "have correct node" taggedAs(Heather) in {
      tester.test(poverty) should be (successful)
    }

  }

  {//1 Increase
    val sent6 = "There will be a small increase in crop diversity due to the need to combat the climate and market risks as both of these might become more volatile in the future."
    val tester = new GraphTester(sent6)

    val cropDiversity = NodeSpec("crop diversity", Inc("increase", "small"))

    behavior of "Raps_sent6"

    passingTest should "have correct node" taggedAs(Heather) in {
      tester.test(cropDiversity) should be (successful)
    }

  }

  {//1 Increase, 2 Decrease Attachments
    val sent7 = "Significant decline in poverty will be associated with a decrease in family size and increase in non-farm income."
    val tester = new GraphTester(sent7)

    val poverty = NodeSpec("poverty", Dec("decline", "Significant"))
    val familySize = NodeSpec("family size", Dec("decrease"))

    val income = NodeSpec("non-farm income", Inc("increase"))

    behavior of "Raps_sent7"

    passingTest should "have correct node 1" taggedAs(Heather) in {
      tester.test(poverty) should be (successful)
    }

    passingTest should "have correct node 2" taggedAs(Heather) in {
      tester.test(familySize) should be (successful)
    }

    passingTest should "have correct node 3" taggedAs(Heather) in {
      tester.test(income) should be (successful)
    }

  }

  {//1 Increase Attachment
    val sent8 = "Poverty levels continue to increase, people become more vulnerable to food insecurity and other risks."
    val tester = new GraphTester(sent8)

    val poverty = NodeSpec("Poverty levels", Inc("increase"))

    behavior of "Raps_sent8"

    passingTest should "have correct node" taggedAs(Heather) in {
      tester.test(poverty) should be (successful)
    }

  }

  {//1 Increase, 1 (maybe?) Causal
    val sent9 = "Government puts more emphasis on improving the agricultural water irrigation/management system to cope with drought conditions."
    val tester = new GraphTester(sent9)

    val gov = NodeSpec("Government")
    val waterSystem = NodeSpec("agricultural water irrigation/management system", Inc("improving"))

    behavior of "Raps_sent9"

    //extracts a Causal event, but "puts more emphasis" doesn't seem like a true Causal to me../
    futureWorkTest should "have correct edge" taggedAs(Heather) in {
      tester.test(EdgeSpec(gov, Causal, waterSystem)) should be (successful)
    }

  }

  {//1 Inc, 1 Dec, 2 Causal
    // modified sentence to remove "drought-/" and add "a" after "with"
    val sent10 = "The government promotes high-yielding and flood-tolerant rice varieties with a policy to encourage the application of organic fertilizers, decreasing the cost on inorganic fertilizers."
    val tester = new GraphTester(sent10)

    val gov = NodeSpec("government")
    val rice = NodeSpec("high-yielding and flood-tolerant rice varieties", Inc("promotes"))
    val policy = NodeSpec("policy to encourage the application of organic fertilizers")
    val fertPriceDown = NodeSpec("cost on inorganic fertilizers", Dec("decreasing"))

    behavior of "Raps_sent10"

    failingTest should "have correct edge 1" taggedAs(Heather) in {
      tester.test(EdgeSpec(gov, Causal, rice)) should be (successful)
    }
    passingTest should "have correct edge 2" taggedAs(Heather) in {
      tester.test(EdgeSpec(gov, Causal, fertPriceDown)) should be (successful)
    }
    failingTest should "have correct edge 3" taggedAs(Heather) in {
      tester.test(EdgeSpec(policy, Causal, rice)) should be (successful)
    }

  }

  {//5-ish increase
  val sent11 = "Use of improved cultivars and mechanization will be increased and use of critical interventions may lead to increases in productivity and efficient use of resources."
    val tester = new GraphTester(sent11)

    val use = NodeSpec("Use", Inc("increased"))
    val cultivars = NodeSpec("Use of improved cultivars and mechanization", Pos("improved"))
    val mechanization = NodeSpec("mechanization", Pos("improved"))
    val interventions = NodeSpec("use of critical interventions", Quant("critical"))
    val productivity = NodeSpec("productivity", Inc("increases"))
    val resources = NodeSpec("efficient use of resources", Inc("increases"))

    behavior of "Raps_sent11"

    // Not expanding non-args
    //This should probably be "Use of improved cultivars" for the NodeSpec
//    inferenceTest should "have correct node 1" in {
//      tester.test(use) should be (successful)
//    }

    passingTest should "have correct node 2" in {
      tester.test(cultivars) should be (successful)
    }

    tempBrokenEntitiesTest should "have correct node 3" in {
      tester.test(mechanization) should be (successful)
    }

    tempBrokenEntitiesTest should "have correct edge 1" in {
      tester.test(EdgeSpec(interventions, Causal, productivity)) should be (successful)
    }

    tempBrokenEntitiesTest should "have correct edge 2" in {
      tester.test(EdgeSpec(interventions, Causal, resources)) should be (successful)
    }

  }

  { //2 Increase, 1 Causal
    val sent12 = "Therefore, the government is committed to supporting the agriculture sector through increased public investment to fulfill the needs of an increasing population."
    val tester = new GraphTester(sent12)

    val gov = NodeSpec("government")
    val sector = NodeSpec("agriculture sector", Inc("supporting"))
    val investment = NodeSpec("increased public investment", Inc("increased"))
    val population = NodeSpec("needs of an increasing population", Inc("increasing"))

    behavior of "Raps_sent12"

    passingTest should "have correct node 1" in {
      tester.test(population) should be (successful)
    }

    passingTest should "have correct node 2" in {
      tester.test(investment) should be (successful)
    }

    failingTest should "have correct edge" in {
      tester.test(EdgeSpec(gov, Causal, sector)) should be (successful)
    }

  }

  { //2 Dec, 1 Inc, 5 causal
    //No Inc or Dec attachments are captured here
    val sent13 = "Water quality and water availability for agriculture will decrease due to pollution of water bodies, and competition for water from other sources," +
      " but water-use efficiency in agriculture will increase due to technological progress."
    val tester = new GraphTester(sent13)

    val waterQuality = NodeSpec("Water quality", Dec("decrease"))
    val waterAvail = NodeSpec("water availability for agriculture", Dec("decrease"))
    val waterEf = NodeSpec("water-use efficiency in agriculture", Inc("increase"))

    val competition = NodeSpec("competition for water from other sources")
    val pollution = NodeSpec("pollution of water bodies")
    val tech = NodeSpec("technological progress")

    behavior of "Raps_sent13"

    failingTest should "have correct edge 1" in {
      tester.test(EdgeSpec(waterQuality, Causal, pollution)) should be (successful)
    }

    failingTest should "have correct edge 2" in {
      tester.test(EdgeSpec(waterAvail, Causal, pollution)) should be (successful)
    }

    failingTest should "have correct edge 3" in {
      tester.test(EdgeSpec(waterQuality, Causal, competition)) should be (successful)
    }

    failingTest should "have correct edge 4" in {
      tester.test(EdgeSpec(waterAvail, Causal, competition)) should be (successful)
    }

    failingTest should "have correct edge 5" in {
      tester.test(EdgeSpec(waterEf, Causal, tech)) should be (successful)
    }

  }

  { //2 Increase Attachments
    val sent14 = "Farm size and wage rates will increase."
    val tester = new GraphTester(sent14)

    val farmSize = NodeSpec("Farm size", Inc("increase"))
    val wageRates = NodeSpec("wage rates", Inc("increase"))

    behavior of "Raps_sent14"

    failingTest should "have correct node 1" in {
      tester.test(farmSize) should be (successful)
    }

    failingTest should "have correct node 2" in {
      tester.test(wageRates) should be (successful)
    }
  }

  {//2 Increase attachments; same issue as sent14 -- conjunction not captured
    val sent15 = "Mechanization and energy-use intensity in agriculture will increase."
    val tester = new GraphTester(sent15)

    val mechanization = NodeSpec("Mechanization", Inc("increase"))
    val energy = NodeSpec("energy-use intensity in agriculture", Inc("increase"))

    behavior of "Raps_sent15"

    failingTest should "have correct node 1" in {
      tester.test(mechanization) should be (successful)
    }

    failingTest should "have correct node 2" in {
      tester.test(energy) should be (successful)
    }

  }

  {//2 Increase attachments: same issue as sent14 and sent15
    val sent16 = "Fertilizer-use intensity and fertilizer productivity will increase."
    val tester = new GraphTester(sent16)

    val fertUse = NodeSpec("Fertilizer-use intensity", Inc("increase"))
    val fertProductivity = NodeSpec("fertilizer productivity", Inc("increase"))

    behavior of "Raps_sent16"

    failingTest should "have correct node 1" in {
      tester.test(fertUse) should be (successful)
    }

    failingTest should "have correct node 2" in {
      tester.test(fertProductivity) should be (successful)
    }

  }

  {//1 Increase, 1 Causal
    val sent17 = "There will not be significant changes in food imports, while yield of important crops will increase due to technological progress in agriculture."
    val tester = new GraphTester(sent17)

    val cropYield = NodeSpec("yield of important crops", Inc("increase"), Quant("important"))
    val tech = NodeSpec("technological progress in agriculture")

    //Currently, it extracts ("yield", Causal, tech) //tech causes yield
    //AND it extracts ("crops", Causal, tech) //tech causes important crops
    //however it really should do ("yield of important crops", Causal, tech)

    behavior of "Raps_sent17"

    failingTest should "have correct edge" in {
      tester.test(EdgeSpec(cropYield, Causal, tech)) should be (successful)
    }

  }

  { //
    val sent18 = "However, opportunities for massive increases in agricultural production and productivity exist but " +
      "are not being exploited."
    val tester = new GraphTester(sent18)

    val production = NodeSpec("agricultural production", Inc("increases", "massive"))
    val productivity = NodeSpec("productivity", Inc("increases", "massive"))

    behavior of "Raps_sent18"

    failingTest should "have correct node 1" in {
      tester.test(production) should be (successful)
    }

    failingTest should "have correct node 2" in {
      tester.test(productivity) should be (successful)
    }

  }

  { //5 increase attachments
    val sent19 = "The governmental policy objective is to achieve food security, ensure adequate raw materials for the manufacturing sector, " +
      "and increased export earnings through increased productivity, efficient input use, and better market access," +
      " infrastructure, and service development."
    val tester = new GraphTester(sent19)

    val productivity = NodeSpec("increased productivity", Inc("increased"))
    val use = NodeSpec("efficient input use", Inc("increased"))
    val marketAccess = NodeSpec("market access", Inc("increased"), Quant("better"))
    //instead of attaching "better" as a quant, Eidos makes a 2nd "market access" entity with "better" as Increase attachment
    //val marketAccess2 = NodeSpec("market access", Inc("better")) //should be quantifier...
    val infrastructure = NodeSpec("infrastructure", Inc("increased"))
    val dev = NodeSpec("service development", Inc("increased"))

    behavior of "Raps_sent19"

    passingTest should "have correct node 1" in {
      tester.test(productivity) should be (successful)
    }

    passingTest should "have correct node 2" in {
      tester.test(use) should be (successful)
    }

    failingTest should "have correct node 3" in {
      tester.test(marketAccess) should be (successful)
    }

    passingTest should "have correct node 4" in {
      tester.test(infrastructure) should be (successful)
    }

    passingTest should "have correct node 5" in {
      tester.test(dev) should be (successful)
    }

  }


  
} //END OF TEST BRACE


