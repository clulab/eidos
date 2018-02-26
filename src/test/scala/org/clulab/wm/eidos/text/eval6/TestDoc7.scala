package org.clulab.wm.eidos.text.eval6

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.Causal
import org.clulab.wm.eidos.text.Correlation
import org.clulab.wm.eidos.text.IsA
import org.clulab.wm.eidos.text.Dec
import org.clulab.wm.eidos.text.EdgeSpec
import org.clulab.wm.eidos.text.Inc
import org.clulab.wm.eidos.text.NodeSpec
import org.clulab.wm.eidos.text.Quant

class TestDoc7 extends Test {
  
  // Filename: FFP Fact Sheet_South Sudan_2018.01.17 BG.pdf
  // Unit test designer: Somebody

  { // Paragraph 1
    val text = """
      After nearly four years of civil conflict, South Sudan remains one of the
      most food-insecure countries in the world. By the end of the 2017 lean
      season in September--the period of the year when food is most
      scarce--approximately 56 percent of the country's population was facing
      life-threatening hunger and in need of humanitarian assistance, making
      2017 the most food-insecure year in South Sudan's history.
      """

    val tester = new Tester(text)

    val conflict = NodeSpec("conflict")
    val food = NodeSpec("food", Quant("scarce", "most"))
    val hunger = NodeSpec("hunger", Quant("life-threatening"))
    val leanSeason = NodeSpec("lean season")

    behavior of "TestDoc7 Paragraph 1"

    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(conflict) should be (successful) 
    }
    failingTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(hunger) should be (successful) 
    }
    failingTest should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(food, Correlation, leanSeason)) should be (successful) 
    }
  }
  { // Paragraph 2
    val text = """
      Despite slight improvements in food availability due to seasonal harvests
      from October to December, the 2018 lean seasons began in January-three
      months earlier than usual-according to the Integrated Food Security Phase
      Classification (IPC). Food security is expected to deteriorate through
      March, with an estimated 5.1 million people-nearly half of the
      population facing Crisis (IPC 3) or worse levels of acute food insecurity.*
      """
  
    val tester = new Tester(text)

    val foodAvailability = NodeSpec("food availability",
                                    Inc("improvements", "slight"))
    val seasonalHarvests = NodeSpec("seasonal harvests")
    val leanSeasons = NodeSpec("lean seasons")
    val foodSecurity = NodeSpec("Food security", Dec("deteriorate"))
    val foodInsecurity = NodeSpec("levels of acute food insecurity", Quant("worse"))
    
    behavior of "TestDoc7 Paragraph 2"

    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(foodSecurity) should be (successful) 
    }
    failingTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(foodInsecurity) should be (successful) 
    }
    failingTest should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(seasonalHarvests, Causal, foodAvailability)) should be (successful) 
    }
  }
  { // Paragraph 3
    val text = """
      In particular, ongoing conflict has severely affected areas of Western
      Bahr El Ghazal State, resulting in approximately 20,000 people
      experiencing Humanitarian Catastrophe levels of acute food insecurity--or
      famine at the household level--meaning that starvation, destitution and
      death are evident
      """

    val tester = new Tester(text)

    val conflict = NodeSpec("conflict", Quant("ongoing"))
    val foodInsecurity = NodeSpec("levels of acute food insecurity")
    val starvation = NodeSpec("starvation", Quant("evident"))
    val destitution = NodeSpec("destitution", Quant("evident"))
    val death = NodeSpec("death", Quant("evident"))

    behavior of "TestDoc7 Paragraph 3"

    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(conflict) should be (successful) 
    }
    failingTest should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(foodInsecurity, Correlation, starvation)) should be (successful) 
    }
    failingTest should "have correct edge 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(foodInsecurity, Correlation, destitution)) should be (successful) 
    }
    failingTest should "have correct edge 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(foodInsecurity, Correlation, death)) should be (successful) 
    }
  }
  { // Paragraph 4
    val text = """
      As of January 2017, approximately 2.4 million refugees have fled South
      Sudan for neighboring countries and another 1.9 million South Sudanese
      remain internally displaced. Widespread insecurity continues to displace
      communities, disrupt livelihood activities, exacerbate food insecurity and
      impede humanitarian access to vulnerable populations.
      """
  
    val tester = new Tester(text)

    val refugees = NodeSpec("refugees")
    val insecurity = NodeSpec("insecurity", Quant("Widespread"))
    val communities = NodeSpec("communities", Quant("displace"))
    val livelihoodActivities = NodeSpec("livelihood activities", Quant("disrupt"))
    val foodInsecurity = NodeSpec("food insecurity", Quant("exacerbate"))
    val humanitarianAccess = NodeSpec(
      "humanitarian access to vulnerable populations", Quant("impede"))
    
    behavior of "TestDoc7 Paragraph 4"

    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(refugees) should be (successful) 
    }
    failingTest should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, communities)) should be (successful) 
    }
    failingTest should "have correct edge 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, livelihoodActivities)) should be (successful) 
    }
    failingTest should "have correct edge 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, foodInsecurity)) should be (successful) 
    }
    failingTest should "have correct edge 4" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, humanitarianAccess)) should be (successful) 
    }
  }
  { // Paragraph 5
    val text = """
      A sustained and unimpeded humanitarian response is critical to saving
      lives and preventing a deterioration to Famine (IPC 5) levels of acute
      food insecurity. Since the start of the conflict, the USAID's Office of
      Food for Peace (FFP) and its partners including the UN World Food Program
      (WFP) and the UN Children's Fund (UNICEF)-have responded to the needs of
      South Sudan's most vulnerable and conflict-affected populations through
      emergency food and nutrition interventions. In FY 2017, FFP-supported
      programs provided life-saving food assistance to 1.1 million people per
      month, on average.
      """
  
    val tester = new Tester(text)

    val humanitarianResponse = NodeSpec("humanitarian response",
                                        Quant("sustained", "unimpeded"))
    val foodInsecurity = NodeSpec("levels of acute food insecurity",
                                  Quant("deterioration"))
    
    behavior of "TestDoc7 Paragraph 5"

    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(humanitarianResponse) should be (successful) 
    }
    failingTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(foodInsecurity) should be (successful) 
    }
  }
  { // Paragraph 6
    val text = """
	  FFP also partners with Catholic Relief Services to provide families in Jonglei
	  State with emergency food assistance, expand access to safe drinking water, and
	  implement livelihoods interventions, including providing agricultural training
	  for farming households.
      """
  
    val tester = new Tester(text)

    val foodAssistance = NodeSpec("emergency food assistance")
    val water = NodeSpec("access to safe drinking water", Inc("expand"))
    val livelihoodsInterventions = NodeSpec("livelihoods interventions")
    val agriculturalTraining = NodeSpec("agricultural training")

    behavior of "TestDoc7 Paragraph 6"

    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(foodAssistance) should be (successful)
    }
    failingTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(water) should be (successful) 
    }
    failingTest should "have correct singleton node 3" taggedAs(Somebody) in {
      tester.test(livelihoodsInterventions) should be (successful) 
    }
    futureWorkTest should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(agriculturalTraining, IsA, livelihoodsInterventions)) should be (successful) 
    }
  }
}
