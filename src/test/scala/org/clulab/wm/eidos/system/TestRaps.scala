package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.{Inc, Dec, Quant, Causal, Origin, TransparentLink, EdgeSpec, NodeSpec}


class TestRaps extends Test {


  { //One Increase Event
    val sent1 = "Better and well-functioning agricultural credit and market services for both established and emerging farmers."
    val tester = new Tester(sent1)

    val credit = NodeSpec("well-functioning agricultural credit", Inc("Better") )

    behavior of "Raps_sent1"

    passingTest should "have the correct node" taggedAs(Heather) in {
      tester.test(credit) should be (successful)

    }

  }

  { //3 Increase events
    val sent2 = "The support for agricultural research, education, and extension programs will also be increased for developing and disseminating climate change adaptation agricultural technologies to the farmers."
    // Note: parse of sentence makes it "impossible" to extract increase for education and extension programs
    // Maybe a reason to switch to cluprocessor
    val tester = new Tester(sent2)

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

  { //3 Decrease, 2 Increase Events; 2 Causal Edges, 1 Origin Edge
    val sent3 = "Limited financial capacities and low education levels further restrict farmers' ability for higher benefits from increased agricultural production."
    val tester = new Tester(sent3)

    val financial = NodeSpec("Limited financial capacities", Dec("Limited"))
    val ability = NodeSpec("\' ability", Dec("restrict"))
    val education = NodeSpec("education levels", Dec("low"), Quant("low"))

    val production = NodeSpec("agricultural production", Inc("increased"))
    val benefits = NodeSpec("benefits", Inc("higher"))

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

  { //3 Increase, 7 Decrease Events, 10 Causal
    val sent4 = "The government promotes improved cultivar and climate-smart technologies but the policy to cut down the use of inorganic fertilizer " +
      "and phase out the fertilizer subsidy results in deteriorating biophysical conditions, low use of inorganic fertilizer, less water, reduced farm sizes which lead to low benefit from the improved cultivar."

    val tester = new Tester(sent4)
    //increase
    val cultivar1 = NodeSpec("cultivar", Inc("improved"), Inc("promotes"))
    val cultivar2 = NodeSpec("cultivar", Inc("improved"))
    val tech = NodeSpec("climate-smart technologies", Inc("improved"), Inc("promotes"))

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
    failingTest should "have correct edges 4" taggedAs(Heather) in {
      tester.test(EdgeSpec(subsidy, Causal, conditions)) should be (successful)
    }

    //phase out subsidy results in ... low use of inorganic fertilizer
    failingTest should "have correct edges 5" taggedAs(Heather) in {
      tester.test(EdgeSpec(subsidy, Causal, fertUse2)) should be (successful)
    }

    //phase out results in ... less water
    failingTest should "have correct edges 6" taggedAs(Heather) in {
      tester.test(EdgeSpec(subsidy, Causal, water)) should be (successful)
    }

    //phase out results in ... reduced farm size
    failingTest should "have correct edges 7" taggedAs(Heather) in {
      tester.test(EdgeSpec(subsidy, Causal, farmSize)) should be (successful)
    }

    //low use of inorganic fertilizer... which lead to low benefit
    failingTest should "have correct edges 8" taggedAs(Heather) in {
      tester.test(EdgeSpec(fertUse2, Causal, benefit)) should be (successful)
    }

    //less water ... lead to low benefit
    failingTest should "have correct edges 9" taggedAs(Heather) in {
      tester.test(EdgeSpec(water, Causal, benefit)) should be (successful)
    }

    //reduced farm sizes which lead to low benefit from the improved cultivar
    passingTest should "have correct edges 10" taggedAs(Heather) in {
      tester.test(EdgeSpec(farmSize, Causal, benefit)) should be (successful)
    }

  }

  {//1 Increase
    val sent5 = "With increases in poverty levels people become more vulnerable to climate change and other risks."
    val tester = new Tester(sent5)

    val poverty = NodeSpec("poverty levels", Inc("increases"))

    behavior of "Raps_sent5"

    passingTest should "have correct node" taggedAs(Heather) in {
      tester.test(poverty) should be (successful)
    }

  }

  {//1 Increase
    val sent6 = "There will be a small increase in crop diversity due to the need to combat the climate and market risks as both of these might become more volatile in the future."
    val tester = new Tester(sent6)

    val cropDiversity = NodeSpec("crop diversity", Inc("increase"), Quant("small"))
    //crop diversity Attachments: Increase(increase,Some(ArraySeq(small)))
    //"small" not added as Quant attachment, however it is identified seperately
    //List(Quantifier) => small

    behavior of "Raps_sent6"

    futureWorkTest should "have correct node" taggedAs(Heather) in {
      tester.test(cropDiversity) should be (successful)
    }

  }

  {//1 Increase, 2 Decrease Events
    val sent7 = "Significant decline in poverty will be associated with a decrease in family size and increase in non-farm income."
    val tester = new Tester(sent7)

    val poverty = NodeSpec("poverty", Dec("decline"), Quant("Significant"))
    val familySize = NodeSpec("family size", Dec("decrease"))

    val income = NodeSpec("non-farm income", Inc("increase"))

    behavior of "Raps_sent7"

    //fails for same reason as sent6 test. Quant("Significant") is Some(ArraySeq(Significant))) in Poverty Entity
    futureWorkTest should "have correct node 1" taggedAs(Heather) in {
      tester.test(poverty) should be (successful)
    }

    passingTest should "have correct node 2" taggedAs(Heather) in {
      tester.test(familySize) should be (successful)
    }

    passingTest should "have correct node 3" taggedAs(Heather) in {
      tester.test(income) should be (successful)
    }

  }

  {//1 Increase Event
    val sent8 = "Poverty levels continue to increase, people become more vulnerable to food insecurity and other risks."
    val tester = new Tester(sent8)

    val poverty = NodeSpec("Poverty levels", Inc("increase"))

    behavior of "Raps_sent8"

    passingTest should "have correct node" taggedAs(Heather) in {
      tester.test(poverty) should be (successful)
    }

  }

  {//1 Increase, 1 (maybe?) Causal
    val sent9 = "Government puts more emphasis on improving the agricultural water irrigation/management system to cope with drought conditions."
    val tester = new Tester(sent9)

    val gov = NodeSpec("Government")
    val waterSystem = NodeSpec("agricultural water irrigation/management system", Inc("improving"))

    behavior of "Raps_sent9"

    //extracts a Causal event, but "puts more emphasis" doesn't seem like a true Causal to me../
    futureWorkTest should "have correct edge" taggedAs(Heather) in {
      tester.test(EdgeSpec(gov, Causal, waterSystem)) should be (successful)
    }

  }

  {//1 Inc, 1 Dec, 2 Causal
    val sent10 = "The government promotes high-yielding and drought-/flood-tolerant rice varieties with policy to encourage the application of organic fertilizers, decreasing the cost on inorganic fertilizers."
    val tester = new Tester(sent10)

    val gov = NodeSpec("government")
    //system extracts val drought = NodeSpec("drought", Inc("promotes")), although drought is not Increase
    val rice = NodeSpec("flood-tolerant rice varieties", Inc("high-yielding"), Inc("promotes"))
    val fertPriceUp = NodeSpec("cost on inorganic fertilizers", Inc("promotes")) //wrong!!!
    val fertPriceDown = NodeSpec("cost on inorganic fertilizers", Dec("decreasing"))

    behavior of "Raps_sent10"

    passingTest should "have correct edge 1" taggedAs(Heather) in {
      tester.test(EdgeSpec(gov, Causal, rice)) should be (successful)
    }

    //incorrectly says gov causes increase in fertprice
    failingTest should "have correct edge 2" taggedAs(Heather) in {
      tester.test(EdgeSpec(gov, Causal, fertPriceUp)) shouldNot be (successful)
    }

    passingTest should "have correct edge 3" taggedAs(Heather) in {
      tester.test(EdgeSpec(gov, Causal, fertPriceDown)) should be (successful)
    }

    //incorrectly says gov causes increase in drought
    failingTest should "have correct edge 4" taggedAs(Heather) in {
      tester.test(EdgeSpec(gov, Causal, NodeSpec("drought", Inc("promotes")))) shouldNot be (successful)
    }

  }

  
} //END OF TEST BRACE


