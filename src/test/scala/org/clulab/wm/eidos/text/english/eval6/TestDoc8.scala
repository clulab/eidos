package org.clulab.wm.eidos.text.english.eval6

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.graph._

// Document: i8533en.pdf
// Unit test designer: Adarsh

class TestDoc8 extends EnglishTest {
  
  { // Paragraph 1
    val text = """
      In South Sudan, the risk of famine remains elevated and food security is
      expected to worsen in 2018. It is critical to enable rapid food production
      among the most vulnerable communities, protect their livelihoods and build
      their resilience, while increasing self-sufficiency.
      """
    val tester = new GraphTester(text)
  
    val famine = NodeSpec("risk of famine", Quant("elevated"))
    val foodSecurity = NodeSpec("food security", Dec("worsen"))
    val foodProduction = NodeSpec("food production", Quant("rapid"))
    val communities = NodeSpec("communities", Quant("vulnerable", "most"))
    val selfSufficiency = NodeSpec("self-sufficiency", Inc("increasing"))
    
    behavior of "TestDoc8 Paragraph 1"
  // We're not expanding non-arg nodes
//    failingTest should "have correct singleton node 1" taggedAs(Becky) in {
//      tester.test(famine) should be (successful)
//    }
//    failingTest should "have correct singleton node 2" taggedAs(Becky) in {
//      tester.test(foodSecurity) should be (successful)
//    }
//    failingTest should "have correct singleton node 3" taggedAs(Becky) in {
//      tester.test(foodProduction) should be (successful)
//    }
//    passingTest should "have correct singleton node 4" taggedAs(Somebody) in {
//      tester.test(communities) should be (successful)
//    }
//    passingTest should "have correct singleton node 5" taggedAs(Somebody) in {
//      tester.test(selfSufficiency) should be (successful)
//    }
  }
  { // Objective 1
    val text = """
      Provide emergency support to protect and rebuild livelihoods during the
      main planting and dry seasons to improve the food security of vulnerable
      urban and rural populations and reduce the food gap.
      """
    val tester = new GraphTester(text)
  
    val emergencySupport = NodeSpec("emergency support to protect and rebuild livelihoods during the main planting and dry seasons", Inc("Provide"))
    val foodSecurity = NodeSpec("food security of vulnerable urban and rural populations", Inc("improve"), Quant("vulnerable"))
    val foodGap = NodeSpec("food gap", Dec("reduce"))
    
    behavior of "TestDoc8 Objective 1"

    failingTest should "have correct edge 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(emergencySupport, Causal, foodSecurity)) should be (successful) 
    }
    failingTest should "have correct edge 2" taggedAs(Ajay) in {
      tester.test(EdgeSpec(emergencySupport, Causal, foodGap)) should be (successful) 
    }
  }
  { // Objective 2
    val text = """
      Strengthen the collection, analysis and coordination of food security and
      agriculture information.
      """
    val tester = new GraphTester(text)
  
    val agricultureInformation = NodeSpec("agriculture information")
    
    behavior of "TestDoc8 Objective 2"

    // Removed.  Since it doesn't participate in a causal event and is not modified, we prune the node out.
//    passingTest should "have correct singleton node 1" taggedAs(Keith) in {
//      tester.test(agricultureInformation) should be (successful)
//    }
  }

  { // Sidenote
    val text = """
      Worsening food security trends linked to continued conflict have been
      compounded by market failure, internal displacement and decreasing
      humanitarian access. To save lives in the coming year, the most vulnerable
      households need support to produce nutritious food for consumption and sale.
      """
    val tester = new GraphTester(text)
  
    val foodSecurityTrends = NodeSpec("Worsening food security trends", Dec("Worsening"), Inc("compounded"))
    val conflict = NodeSpec("continued conflict")

    val marketFailure = NodeSpec("market", Dec("failure"))
    val internalDisplacement = NodeSpec("internal displacement")
    val access = NodeSpec("humanitarian access", Dec("decreasing"))

    behavior of "TestDoc8 Sidenote"

    failingTest should "have correct edge 1" taggedAs(Keith) in {
      tester.test(EdgeSpec(conflict, Correlation, foodSecurityTrends)) should be(successful)
    }
    tempBrokenEntitiesTest should "have correct edge 2" taggedAs(Keith) in {
      tester.test(EdgeSpec(marketFailure, Causal, foodSecurityTrends)) should be (successful) 
    }
    tempBrokenEntitiesTest should "have correct edge 3" taggedAs(Keith) in {
      tester.test(EdgeSpec(internalDisplacement, Causal, foodSecurityTrends)) should be (successful) 
    }
    tempBrokenEntitiesTest should "have correct edge 4" taggedAs(Keith) in {
      tester.test(EdgeSpec(access, Causal, foodSecurityTrends)) should be (successful)
    }
  }

  { // Impact paragraph 1
    val text = """
      Humanitarian response succeeded in containing famine soon after it was
      declared in February 2017. However, there is an elevated risk of famine in
      2018 if widespread support is not continued. From January to March 2018,
      the number of severely food insecure people is likely to climb to 5.1
      million people. This follows the trend seen since conflict began--rates
      of food insecurity accelerating even during the harvest season. Food
      insecurity levels worsen further with each lean season, a time of year
      when food stocks are typically depleted, food prices are at their highest
      and heavy rains disrupt markets and restrict humanitarian access. In 2018,
      the lean season is expected to start three months earlier than normal.
      Malnutrition levels remain well above emergency thresholds in some areas.
      """
    val tester = new GraphTester(text)
  
    val humanitarianResponse = NodeSpec("Humanitarian response")
    val famineContainment = NodeSpec("famine", Dec("containing"))
    val famineRisk = NodeSpec("risk of famine", Quant("elevated"))
    val supportNotContinued = NodeSpec("support", Quant("widespread"), Dec("not continued"))
    val noOfFIpplClimb = NodeSpec("number of severely food insecure people", Inc("climb"))
    val foodInsRates = NodeSpec("rates of food insecurity", Inc("accelerating"))
    val foodInsLevels = NodeSpec("Food insecurity levels", Dec("worsen"))
    val leanSeason = NodeSpec("lean season")
    val foodStocksDepleted = NodeSpec("food stocks", Quant("depleted"))
    val foodPricesHighest = NodeSpec("food prices", Quant("at their highest"))
    val heavyRains = NodeSpec("rains", Quant("heavy"))
    val marketDisruption = NodeSpec("market", Quant("disrupt"))
    val restrictedHumanitarianAccess = NodeSpec("humanitarian access", Dec("restrict"))
    val malnutritionLevels = NodeSpec("Malnutrition levels", Quant("well above emergency thresholds"))
    
    behavior of "TestDoc8 Impact para 1"

    failingTest should "have correct edge 1" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(humanitarianResponse, Causal, famineContainment)) should be (successful) 
    }
    failingTest should "have correct edge 2" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(supportNotContinued, Causal, famineRisk)) should be (successful) 
    }
    failingTest should "have correct singleton node 1" taggedAs(Adarsh) in {
      tester.test(noOfFIpplClimb) should be (successful) 
    }
    failingTest should "have correct edge 3" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(leanSeason, Causal, foodInsLevels)) should be (successful) 
    }
    failingTest should "have correct edge 4" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(leanSeason, Correlation, foodStocksDepleted)) should be (successful) 
    }
    failingTest should "have correct edge 5" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(leanSeason, Correlation, foodPricesHighest)) should be (successful) 
    }
    failingTest should "have correct edge 6" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(heavyRains, Causal, marketDisruption)) should be (successful) 
    }
    failingTest should "have correct edge 7" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(heavyRains, Causal, restrictedHumanitarianAccess)) should be (successful) 
    }
    failingTest should "have correct singleton node 2" taggedAs(Adarsh) in {
      tester.test(malnutritionLevels) should be (successful)
    }
  }
  { // Impact paragraph 2
    val text = """
      Persistent insecurity and massive displacement have led to widespread disruption
      of livelihood activities, including agricultural, fisheries and livestock
      production, as well as limited access to local food markets among producers,
      traders and consumers. 

      Exacerbated by climate extremes and a reduction in planted
      area, the national cereal gap has widened.

      The shortfall of about half a million tonnes in 2017 is likely to be
      similar in 2018.

      Trade and migration routes have been disrupted, and the risk and
      occurrence of livestock disease outbreaks has increased.

      Economic crisis, marked by drastic hyperinflation and market failures, has
      further destabilised food systems and household access to food and income.

      Supporting local food production will remain critical to preventing
      further deterioration of the food security situation in 2018.
      """
    val tester = new GraphTester(text)
  
    val persistentInsecurity = NodeSpec("Persistent insecurity", Quant("Persistent"))
    val massiveDisplacement  = NodeSpec("massive displacement", Quant("massive"))
    val livelihoodActivities = NodeSpec("livelihood activities", Dec("disruption"))
    val accessToMarkets = NodeSpec("access to local food markets", Dec("disruption"), Dec("limited"))

    val cerealGap = NodeSpec("national cereal gap", Inc("widened"))
    val climateExtremes = NodeSpec("climate extremes")
    val areaPlanted = NodeSpec("planted area", Dec("reduction"))

    val routesa = NodeSpec("Trade", Dec("disrupted"))
    val routesb = NodeSpec("migration routes", Dec("disrupted"))
    // How might this be done?
    val routesc = NodeSpec("Trade and migration routes", Dec("disrupted"))
    // We're not this smart yet
    //val routesd = NodeSpec("Trade routes", Dec("disrupted"))

    // How might this be done?
    // We're no longer expanding non-args
//    val riska = NodeSpec("risk of livestock disease outbreaks", Inc("increased"))
//    val riskb = NodeSpec("occurrence of livestock disease outbreaks", Inc("increased"))
    // This is ungrammatical to me.  Risk and occurrence _have_ increased
    //val riskc = NodeSpec("risk and occurrence of livestock disease outbreaks", Inc("increased"))

    val econCrisis = NodeSpec("Economic crisis")
    val hyperinflation = NodeSpec("drastic hyperinflation", Quant("drastic"))
    val marketFailures = NodeSpec("market failures", Dec("failures"))
    val foodSystems = NodeSpec("food systems", Dec("destabilised"))
    val accessToFood = NodeSpec("household access to food", Dec("destabilised"))
    val income = NodeSpec("income", Dec("destabilised"))

    val localFoodProduction = NodeSpec("Supporting local food production", Quant("critical"))
    val foodSecuritySituation = NodeSpec("food security situation", Dec("deterioration"),  Dec("preventing"))
    behavior of "TestDoc8 Impact para 2"

    tempBrokenEntitiesTest should "have correct edge 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(persistentInsecurity, Causal, livelihoodActivities)) should be (successful) 
    }
    tempBrokenEntitiesTest should "have correct edge 2" taggedAs(Ajay) in {
      tester.test(EdgeSpec(massiveDisplacement, Causal, livelihoodActivities)) should be (successful) 
    }
    tempBrokenEntitiesTest should "have correct edge 3" taggedAs(Ajay) in {
      tester.test(EdgeSpec(persistentInsecurity, Causal, accessToMarkets)) should be (successful) 
    }
    tempBrokenEntitiesTest should "have correct edge 4" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(massiveDisplacement, Causal, accessToMarkets)) should be (successful) 
    }
    passingTest should "have correct edge 5" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(climateExtremes, Causal, cerealGap)) should be (successful) 
    }
    passingTest should "have correct edge 6" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(areaPlanted, Causal, cerealGap)) should be (successful) 
    }
    passingTest should "have correct singleton node 1a" taggedAs(Keith) in {
      tester.test(routesa) should be (successful)
    }
    passingTest should "have correct singleton node 1b" taggedAs(Keith) in {
      tester.test(routesb) should be (successful)
    }
    ignore should "have correct singleton node 1c" taggedAs(Keith) in {
      tester.test(routesc) should be (successful)
    }
//    ignore should "have correct singleton node 2a" taggedAs(Keith) in {
//      tester.test(riska) should be (successful)
//    }
//    passingTest should "have correct singleton node 2b" taggedAs(Keith) in {
//      tester.test(riskb) should be (successful)
//    }
    passingTest should "have correct edge 7" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(hyperinflation, Correlation, econCrisis)) should be (successful) 
    }
    passingTest should "have correct edge 8" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(marketFailures, Correlation, econCrisis)) should be (successful) 
    }
    passingTest should "have correct edge 9" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(econCrisis, Causal, foodSystems)) should be (successful) 
    }
    tempBrokenEntitiesTest should "have correct edge 10" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(econCrisis, Causal, accessToFood)) should be (successful) 
    }
    tempBrokenEntitiesTest should "have correct edge 11" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(econCrisis, Causal, income)) should be (successful) 
    }
    passingTest should "have correct singleton node 3" taggedAs(Egoitz) in {
      tester.test(localFoodProduction) should be (successful) 
    }
    passingTest should "have correct singleton node 4" taggedAs(Egoitz) in {
      tester.test(foodSecuritySituation) should be (successful) 
    }
  }

}
