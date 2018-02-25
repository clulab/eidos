package org.clulab.wm.eidos.text.eval6

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.Causal
import org.clulab.wm.eidos.text.Correlation
import org.clulab.wm.eidos.text.Dec
import org.clulab.wm.eidos.text.EdgeSpec
import org.clulab.wm.eidos.text.Inc
import org.clulab.wm.eidos.text.NodeSpec
import org.clulab.wm.eidos.text.Quant

class TestDoc8 extends Test {
  
  { // Paragraph 1
    val text = """
      In South Sudan, the risk of famine remains elevated and food security is
      expected to worsen in 2018. It is critical to enable rapid food production
      among the most vulnerable communities, protect their livelihoods and build
      their resilience, while increasing self-sufficiency.
      """
    val tester = new Tester(text)
  
    val famine = NodeSpec("risk of famine", Quant("elevated"))
    val foodSecurity = NodeSpec("food security", Dec("worsen"))
    val foodProduction = NodeSpec("food production", Quant("rapid"))
    val communities = NodeSpec("communities", Quant("vulnerable"))
    val selfSufficiency = NodeSpec("self-sufficiency", Inc("increasing"))
    
    behavior of "TestDoc8 Paragraph 1"

    ignore should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(famine) should be (successful) 
    }
    ignore should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(foodSecurity) should be (successful) 
    }
    ignore should "have correct singleton node 3" taggedAs(Somebody) in {
      tester.test(foodProduction) should be (successful) 
    }
    ignore should "have correct singleton node 4" taggedAs(Somebody) in {
      tester.test(communities) should be (successful) 
    }
    ignore should "have correct singleton node 5" taggedAs(Somebody) in {
      tester.test(selfSufficiency) should be (successful) 
    }
  }
  { // Objective 1
    val text = """
      Provide emergency support to protect and rebuild livelihoods during the
      main planting and dry seasons to improve the food security of vulnerable
      urban and rural populations and reduce the food gap.
      """
    val tester = new Tester(text)
  
    val emergencySupport = NodeSpec("emergency support")
    val foodSecurity = NodeSpec("food security of vulnerable urban and rural populations", Inc("improve"))
    val foodGap = NodeSpec("food gap", Dec("reduce"))
    
    behavior of "TestDoc8 Objective 1"

    ignore should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(emergencySupport, Causal, foodSecurity)) should be (successful) 
    }
    ignore should "have correct edge 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(emergencySupport, Causal, foodGap)) should be (successful) 
    }
  }
  { // Objective 2
    val text = """
      Strengthen the collection, analysis and coordination of food security and
      agriculture information.
      """
    val tester = new Tester(text)
  
    val agricultureInformation = NodeSpec("agricultural information")
    
    behavior of "TestDoc8 Objective 1"

    ignore should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(agricultureInformation) should be (successful) 
    }
  }

  { // Sidenote
    val text = """
      Worsening food security trends linked to continued conflict have been
      compounded by market failure, internal displacement and decreasing
      humanitarian access. To save lives in the coming year, the most vulnerable
      households need support to produce nutritious food for consumption and sale.
      """
    val tester = new Tester(text)
  
    val foodSecurityTrends = NodeSpec("worsening food security trends", Dec("compounded"))
    val conflict = NodeSpec("conflict", Quant("continued"))
    val marketFailure = NodeSpec("market failure")
    val internalDisplacement = NodeSpec("internal displacement")
    
    behavior of "TestDoc8 Sidenote"

    ignore should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(marketFailure, Causal, foodSecurityTrends)) should be (successful) 
    }
    ignore should "have correct edge 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(internalDisplacement, Causal, foodSecurityTrends)) should be (successful) 
    }
    ignore should "have correct edge 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(conflict, Correlation, foodSecurityTrends)) should be (successful) 
    }
  }
  { // Impact paragraph 1
    val text = """
      Humanitarian response succeeded in containing famine soon after it was
      declared in February 2017. However, there is an elevated risk of famine in
      2018 if widespread support is not continued. From January to March 2018,
      the number of severely food insecure people is likely to climb to 5.1
      million people. This follows the trend seen since conflict began - rates
      of food insecurity accelerating even during the harvest season. Food
      insecurity levels worsen further with each lean season, a time of year
      when food stocks are typically depleted, food prices are at their highest
      and heavy rains disrupt markets and restrict humanitarian access. In 2018,
      the lean season is expected to start three months earlier than normal.
      Malnutrition levels remain well above emergency thresholds in some areas.
      """
    val tester = new Tester(text)
  
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

    ignore should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(humanitarianResponse, Causal, famineContainment)) should be (successful) 
    }
    ignore should "have correct edge 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(supportNotContinued, Causal, famineRisk)) should be (successful) 
    }
    ignore should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(noOfFIpplClimb) should be (successful) 
    }
    ignore should "have correct edge 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(leanSeason, Causal, foodInsLevels)) should be (successful) 
    }
    ignore should "have correct edge 4" taggedAs(Somebody) in {
      tester.test(EdgeSpec(leanSeason, Correlation, foodStocksDepleted)) should be (successful) 
    }
    ignore should "have correct edge 5" taggedAs(Somebody) in {
      tester.test(EdgeSpec(leanSeason, Correlation, foodPricesHighest)) should be (successful) 
    }
    ignore should "have correct edge 6" taggedAs(Somebody) in {
      tester.test(EdgeSpec(heavyRains, Causal, marketDisruption)) should be (successful) 
    }
    ignore should "have correct edge 7" taggedAs(Somebody) in {
      tester.test(EdgeSpec(heavyRains, Causal, restrictedHumanitarianAccess)) should be (successful) 
    }
    ignore should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(malnutritionLevels) should be (successful)
    }
  }
  { // Impact paragraph 2
    val text = """
      Persistent insecurity and massive displacement have led to widespread disruption
      of livelihood activities, including agricultural, fisheries and livestock
      production, as well as limited access to local food markets among producers,
      traders and consumers. 

      Exacerbated by climate extremes and a reduction in area
      planted, the national cereal gap has widened.

      The shortfall of about half a million tonnes in 2017 is likely to be
      similar in 2018.

      Trade and migration routes have been disrupted, and the risk and
      occurrence of livestock disease outbreaks has increased.

      Economic crisis, marked by drastic hyperinflation and market failures, has
      further destabilised food systems and household access to food and income.

      Supporting local food production will remain critical to preventing
      further deterioration of the food security situation in 2018.
      """
    val tester = new Tester(text)
  
    val persistentInsecurity = NodeSpec("insecurity", Quant("persistent"))
    val massiveDisplacement  = NodeSpec("displacement", Quant("massive"))
    val livelihoodActivities = NodeSpec("disruption of livelihood activities", Quant("widespread"))
    val accessToMarkets = NodeSpec("access to local food markets among producers, traders and consumers", Quant("limited"))

    val cerealGap = NodeSpec("national cereal gap", Inc("widened"))
    val climateExtremes = NodeSpec("climate extremes")
    val areaPlanted = NodeSpec("area planted", Dec("reduction"))

    val routes = NodeSpec("Trade and migration routes", Quant("disrupted"))
    val risk = NodeSpec("risk and occurrence of livestock disease outbreaks", Inc("increased"))
    
    val econCrisis = NodeSpec("Economic crisis")
    val hyperinflation = NodeSpec("hyperinflation", Quant("drastic"))
    val marketFailures = NodeSpec("market failures")
    val foodSystems = NodeSpec("food systems", Quant("destabilized"))
    val accessToFoodAndIncome = NodeSpec("household access to food and income", Quant("destabilized"))

    val localFoodProduction = NodeSpec("local food production", Inc("Supporting"))
    val foodSecuritySituation = NodeSpec("food security situation", Dec("deteriorating"))
    behavior of "TestDoc8 Impact para 2"

    ignore should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(persistentInsecurity, Causal, livelihoodActivities)) should be (successful) 
    }
    ignore should "have correct edge 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(massiveDisplacement, Causal, livelihoodActivities)) should be (successful) 
    }
    ignore should "have correct edge 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(persistentInsecurity, Causal, accessToMarkets)) should be (successful) 
    }
    ignore should "have correct edge 4" taggedAs(Somebody) in {
      tester.test(EdgeSpec(massiveDisplacement, Causal, accessToMarkets)) should be (successful) 
    }
    ignore should "have correct edge 5" taggedAs(Somebody) in {
      tester.test(EdgeSpec(climateExtremes, Causal, cerealGap)) should be (successful) 
    }
    ignore should "have correct edge 6" taggedAs(Somebody) in {
      tester.test(EdgeSpec(areaPlanted, Causal, cerealGap)) should be (successful) 
    }
    ignore should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(routes) should be (successful) 
    }
    ignore should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(risk) should be (successful) 
    }
    ignore should "have correct edge 7" taggedAs(Somebody) in {
      tester.test(EdgeSpec(econCrisis, Correlation, hyperinflation)) should be (successful) 
    }
    ignore should "have correct edge 8" taggedAs(Somebody) in {
      tester.test(EdgeSpec(econCrisis, Correlation, marketFailures)) should be (successful) 
    }
    ignore should "have correct edge 9" taggedAs(Somebody) in {
      tester.test(EdgeSpec(econCrisis, Causal, foodSystems)) should be (successful) 
    }
    ignore should "have correct edge 10" taggedAs(Somebody) in {
      tester.test(EdgeSpec(econCrisis, Causal, accessToFoodAndIncome)) should be (successful) 
    }
    ignore should "have correct edge 11" taggedAs(Somebody) in {
      tester.test(EdgeSpec(econCrisis, Causal, accessToFoodAndIncome)) should be (successful) 
    }
    ignore should "have correct singleton node 3" taggedAs(Somebody) in {
      tester.test(localFoodProduction) should be (successful) 
    }
    ignore should "have correct singleton node 4" taggedAs(Somebody) in {
      tester.test(foodSecuritySituation) should be (successful) 
    }
  }

}
