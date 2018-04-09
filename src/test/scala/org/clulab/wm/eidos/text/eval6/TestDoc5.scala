package org.clulab.wm.eidos.text.eval6

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text._

class TestDoc5 extends Test {
  
  { // Paragraph 1
    val text = """
      |Extreme levels of food insecurity persist across South Sudan
      |and nearly one third of the population is in need of emergency
      |food assistance. Further deterioration in food security is
      |likely during an extended lean season (February-July), as
      |widespread insecurity continues to limit livelihoods, disrupt
      |trade, and block humanitarian access. In a worst-case scenario
      |where conflict intensifies and humanitarian access is further
      |limited, Famine (IPC Phase 5), marked by high levels of excess
      |mortality, is possible. Unity State, where displaced households
      |already face an extreme lack of food, is the area of greatest
      |concern. Urgent action to end conflict and increase the size
      |and scope of emergency assistance delivery is critical to save
      |lives over the coming year.
      """

    val insecurity = NodeSpec("levels of food insecurity", Quant("Extreme"))
    val security = NodeSpec("food security", Dec("deterioration", "Further"))
    val insecurity2 = NodeSpec("insecurity", Quant("widespread"))
    val livelihoods = NodeSpec("livelihoods", Dec("limit"))
    val trade = NodeSpec("trade", Dec("disrupt"))
    val access = NodeSpec("humanitarian access", Dec("block"))
    val conflict = NodeSpec("conflict", Inc("intensifies"))
    val access2 = NodeSpec("humanitarian access", Dec("limited", "further"))
    val famine = NodeSpec("Famine (IPC Phase 5)")
    val mortality = NodeSpec("levels of excess mortality", Quant("high"))
    val food = NodeSpec("food", Dec("lack", "extreme"))
    val action = NodeSpec("Urgent action to end the conflict")
    val delivery = NodeSpec("the size and scope of emergency assistance delivery", Inc("increase"))
    val lives = NodeSpec("lives", Inc("save"))

    behavior of "TestDoc5 Paragraph 1"

    val tester = new Tester(text)

    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(insecurity)
    }
    passingTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(food)
    }
    failingTest should "have correct edges 1" taggedAs(Keith) in {
      tester.test(EdgeSpec(security, Correlation, insecurity2)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Keith) in {
      tester.test(EdgeSpec(insecurity2, Causal, livelihoods)) should be (successful)
    }
    failingTest should "have correct edges 3" taggedAs(Keith) in {
      tester.test(EdgeSpec(insecurity2, Causal, trade)) should be (successful)
    }
    failingTest should "have correct edges 4" taggedAs(Keith) in {
      tester.test(EdgeSpec(insecurity2, Causal, access)) should be (successful)
    }
    failingTest should "have correct edges 5" taggedAs(Keith) in {
      tester.test(EdgeSpec(conflict, Correlation, famine)) should be (successful)
    }
    failingTest should "have correct edges 6" taggedAs(Keith) in {
      tester.test(EdgeSpec(access, Correlation, famine)) should be (successful)
    }
    failingTest should "have correct edges 7" taggedAs(Keith) in {
      tester.test(EdgeSpec(famine, Correlation, mortality)) should be (successful)
    }
    failingTest should "have correct edges 8" taggedAs(Keith) in {
      tester.test(EdgeSpec(delivery, Causal, lives)) should be (successful)
    }
    failingTest should "have correct edges 9" taggedAs(Keith) in {
      tester.test(EdgeSpec(action, Causal, lives)) should be (successful)
    }
  }

  { // Paragraph 2
    val text = """
      |Since the resurgence of conflict in July 2016, violence has
      |spread to Greater Equatoria, and now affects all regions of
      |South Sudan. Over 450,000 people have fled the country since
      |July, bringing the total number of refugees to 1.3
      |million. Nearly two million people are internally
      |displaced. Across much of the country, household access to food
      |and cash income has declined as conflict has disrupted
      |planting, harvesting, and other livelihood activities. Ongoing
      |crop assessments and key informant information indicate that
      |2016 staple food production is below average in many areas,
      |including the typically surplus-producing areas of Western
      |Equatoria.
      """

    val violence = NodeSpec("violence", Inc("spread"))
    val regions = NodeSpec("all regions of South Sudan")
    val access = NodeSpec("household access to food and cash income", Dec("declined"))
    val conflict = NodeSpec("conflict")
    val planting = NodeSpec("planting", Dec("disrupt"))
    val harvesting = NodeSpec("harvesting", Dec("disrupt"))
    val activities = NodeSpec("other livelihood activities", Dec("disrupt"))
    val production = NodeSpec("staple food production", Quant("below average"))

    behavior of "TestDoc5 Paragraph 2"

    val tester = new Tester(text)

    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(production)
    }
    failingTest should "have correct edges 1" taggedAs(Becky) in {
      tester.test(EdgeSpec(violence, Affect, regions)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Becky) in {
      tester.test(EdgeSpec(access, Correlation, conflict)) should be (successful)
    }
    failingTest should "have correct edges 3" taggedAs(Becky) in {
      tester.test(EdgeSpec(conflict, Causal, planting)) should be (successful)
    }
    failingTest should "have correct edges 4" taggedAs(Becky) in {
      tester.test(EdgeSpec(conflict, Causal, harvesting)) should be (successful)
    }
    failingTest should "have correct edges 5" taggedAs(Becky) in {
      tester.test(EdgeSpec(conflict, Causal, activities)) should be (successful)
    }
  }

  { // Paragraph 3
    val text = """
      |Meanwhile, macroeconomic factors continue to drive exorbitant
      |staple food prices. A substantial decline in oil revenue since
      |2014 has contributed to a sharp drop in both foreign currency
      |reserves and the value of the South Sudanese pound. These
      |factors, along with insecurity along key trade routes, have
      |restricted normal trade flows into South Sudan and from the
      |capital to wider areas of the country. This is occurring at a
      |time when import requirements are higher than usual given
      |below-average harvests. The subsequent reduction in food
      |availability on local markets has driven prices to record
      |levels. As of November 2016, retail sorghum prices in Aweil,
      |Wau, and Juba averaged 49 SSD/kg, four times higher than the
      |previous year and 10 to 15 times higher than November 2013, the
      |month before the initial outbreak of conflict. These high
      |prices, along with declining incomes, have significantly eroded
      |household purchasing power.
      """

    val factors = NodeSpec("macroeconomic factors")
    val prices = NodeSpec("staple food prices", Quant("exorbitant"), Inc("exorbitant"))
    val revenue = NodeSpec("oil revenue", Dec("decline", "substantial"))
    val reserves = NodeSpec("foreign currency reserves", Dec("drop", "sharp"))
    val value = NodeSpec("value of South Sudanese pound", Dec("drop", "sharp"))
    val factors2 = NodeSpec("These factors")
    val insecurity = NodeSpec("insecurity along key trade routes")
    val flows = NodeSpec("normal trade flows", Dec("restricted"))
    val requirements = NodeSpec("import requirements", Inc("higher"), Quant("higher"))
    val harvests = NodeSpec("harvests", Quant("below-average"))
    val availability = NodeSpec("food availability", Dec("reduction", "subsequent"))
    val prices2 = NodeSpec("prices", Quant("record levels"))
    val prices3 = NodeSpec("retail sorghum prices", Quant("higher"), Inc("higher"))
    val prices4 = NodeSpec("prices", Quant("high"))
    val incomes = NodeSpec("incomes", Dec("declining"))
    val power = NodeSpec("household purchasing power", Dec("eroded", "significantly"))

    behavior of "TestDoc5 Paragraph 3"

    val tester = new Tester(text)

    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(prices3)
    }
    passingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(factors, Causal, prices)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Ajay) in {
      tester.test(EdgeSpec(revenue, Causal, reserves)) should be (successful)
    }
    passingTest should "have correct edges 3" taggedAs(Ajay) in {
      tester.test(EdgeSpec(revenue, Causal, value)) should be (successful)
    }
    failingTest should "have correct edges 4" taggedAs(Ajay) in {
      tester.test(EdgeSpec(factors2, Causal, flows)) should be (successful)
    }
    failingTest should "have correct edges 5" taggedAs(Ajay) in {
      tester.test(EdgeSpec(insecurity, Causal, flows)) should be (successful)
    }
    failingTest should "have correct edges 6" taggedAs(Ajay) in {
      tester.test(EdgeSpec(harvests, Causal, requirements)) should be (successful)
    }
    failingTest should "have correct edges 7" taggedAs(Ajay) in {
      tester.test(EdgeSpec(availability, Causal, prices2)) should be (successful)
    }
    failingTest should "have correct edges 8" taggedAs(Ajay) in {
      tester.test(EdgeSpec(prices4, Causal, power)) should be (successful)
    }
    failingTest should "have correct edges 9" taggedAs(Ajay) in {
      tester.test(EdgeSpec(incomes, Causal, power)) should be (successful)
    }
  }

  { // Paragraph 4
    val text = """
      |All regions of South Sudan are in need of significant
      |humanitarian response. Crisis (IPC Phase 3) is widespread and
      |Emergency (IPC Phase 4) outcomes exist in parts of Unity,
      |Western Bahr el Ghazal, Northern Bahr el Ghazal, Central
      |Equatoria, and Western Equatoria (Figure 1). An estimated
      |675,000 people are currently in Emergency (IPC Phase 4) or
      |worse, meaning that they face large gaps in their ability to
      |meet basic food requirements. These populations, particularly
      |children, face a significantly elevated risk of malnutrition
      |and mortality. An additional 2.8 million people are in Crisis
      |(IPC Phase 3). The prevalence of Global Acute Malnutrition
      |(GAM), recorded by SMART surveys conducted between September
      |and November 2016, remained Serious or worse (10 percent or
      |higher) throughout the country during the harvest period.
      """

    val response = NodeSpec("humanitarian response", Quant("significant"))
    val crisis = NodeSpec("Crisis (IPC Phase 3)", Quant("widespread"))
    val gaps = NodeSpec("their ability to meet basic food requirements", Dec("gaps", "large"))
    val risk = NodeSpec("risk of malnutrition and mortality", Quant("elevated", "significantly"))
    val malnutrition = NodeSpec("Global Acute Malnutrition (GAM)", Quant("Serious"), Quant("worse"), Dec("worse"))

    behavior of "TestDoc5 Paragraph 4"

    val tester = new Tester(text)

    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(response)
    }
    passingTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(crisis)
    }
    passingTest should "have correct singleton node 3" taggedAs(Somebody) in {
      tester.test(gaps)
    }
    passingTest should "have correct singleton node 4" taggedAs(Somebody) in {
      tester.test(risk)
    }
    passingTest should "have correct singleton node 5" taggedAs(Somebody) in {
      tester.test(malnutrition)
    }
  }

  { // Paragraph 5
    val text = """
      Food security is expected to deteriorate further during the February to
      July lean season, and to be as severe as, or worse than, last year's lean
      season, when some food security outcomes in Northern Bahr el Ghazal,
      Western Bahr el Ghazal, and Unity States surpassed Emergency (IPC Phase 4)
      or Famine (IPC Phase 5) thresholds (Figure 2). In a worst-case scenario,
      where increased conflict further disrupts livelihoods and limits
      humanitarian assistance, Famine (IPC Phase 5) could occur during 2017.
      """

    val security    = NodeSpec("Food security", Dec("deteriorate"), Dec("worse"), Quant("severe"))
    val outcomes    = NodeSpec("some food security outcomes", Inc("surpassed"))
    val conflict    = NodeSpec("conflict", Inc("increased"))
    val livelihoods = NodeSpec("livelihoods", Dec("disrupts"))
    val assistance  = NodeSpec("humanitarian assistance", Dec("limits"))
    val famine      = NodeSpec("Famine (IPC Phase 5)")

    behavior of "TestDoc5 Paragraph 5"

    val tester = new Tester(text)

    /* NOTES: 
     * - Food security node not picking up the decrease trigger 'worse' and
     *   quantifier 'severe'. Can there be multiple Dec's?
     * - Right now edge 1 seems to say that a decrease in food security is
     *   correlated with an increase in food security outcomes. However, in
     *   this case, the 'surpassing' is a negative result, since it refers to
     *   an increase in IPC level. Should this be picked up by the reader?
     * - Humanitarian assistance is picking up the decrease attachment
     *   'disrupt' instead of 'limits'. How to fix this?
     * - What to pick up: food security outcomes, some food security outcomes,
     *   or 'food security outcomes in Northern Bahr el Ghazal, ...' (also,
     *   cartesian product?)
    */

    failingTest should "have correct edges 1" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(security, Correlation, outcomes)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(conflict, Causal, livelihoods)) should be (successful)
    }
    failingTest should "have correct edges 3" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(conflict, Causal, assistance)) should be (successful)
    }
    failingTest should "have correct edges 4" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(conflict, Correlation, famine)) should be (successful)
    }
  }

  { // Paragraph 6
    val text = """
      Of greatest concern are Guit, Koch, Leer, and Panyijiar counties in Unity
      State. In these areas, Emergency (IPC Phase 4) levels of food insecurity
      observed during the 2016 lean season have likely persisted during the
      typical harvest period, as many households were unable to cultivate. Most
      continue to rely primarily on fish and wild foods to survive. Conflict is
      driving new displacement, putting additional stress on available wild food
      sources. Little to no food assistance was distributed in these counties
      from August to November due to access constraints.
      """

    val insecurity   = NodeSpec("Emergency (IPC Phase 4) levels of food insecurity")
    val households   = NodeSpec("many households were unable to cultivate")
    val conflict     = NodeSpec("Conflict")
    val displacement = NodeSpec("new displacement")
    val stress       = NodeSpec("stress on available wild food sources", Inc("additional"))
    val assistance   = NodeSpec("food assistance", Quant("little"), Quant("no"))
    val constrains   = NodeSpec("access constraints")

    behavior of "TestDoc5 Paragraph 6"

    val tester = new Tester(text)

    /* NOTES:
     * - Perhaps we should add IPC Phase classifications as quantifiers for food
     *   security? They are causing problems.
     * */
    failingTest should "have correct edges 1" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(insecurity, Correlation, households)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(conflict, Causal, displacement)) should be (successful)
    }
    failingTest should "have correct edges 3" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(conflict, Causal, stress)) should be (successful)
    }
    failingTest should "have correct edges 4" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(constrains, Causal, assistance)) should be (successful)
    }
  }

  { // Paragraph 7
    val text = """
      |Other areas of concern include: 1) Western Bahr el Ghazal,
      |where conflict is also limiting agricultural activities,
      |driving displacement, and disrupting normal market functioning;
      |2) Northern Bahr el Ghazal, where prices are extremely high and
      |households are especially dependent on markets to access food;
      |and 3) Greater Equatoria where ongoing conflict has disrupted
      |crop production and restricted the movement of local
      |populations. The forthcoming multi-partner IPC analysis will
      |consider a variety of new information and provide an updated
      |assessment of current and future acute food insecurity.
      """

    val conflict = NodeSpec("conflict")
    val activities = NodeSpec("agricultural activities", Dec("limiting"))
    val displacement = NodeSpec("displacement")
    val functioning = NodeSpec("normal market functioning", Dec("disrupting"))
    val prices = NodeSpec("prices", Quant("extremely high"))
    val conflict2 = NodeSpec("conflict", Quant("ongoing"))
    val production = NodeSpec("crop production", Dec("disrupted"))
    val movement = NodeSpec("movement of local populations", Dec("restricted"))

    behavior of "TestDoc5 Paragraph 7"

    val tester = new Tester(text)

    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(prices)
    }
    failingTest should "have correct edges 1" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(conflict, Causal, activities)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(conflict, Causal, displacement)) should be (successful)
    }
    failingTest should "have correct edges 3" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(conflict, Causal, functioning)) should be (successful)
    }
    passingTest should "have correct edges 4" taggedAs(Somebody) in {
      tester.test(EdgeSpec(conflict2, Causal, production)) should be (successful)
    }
    failingTest should "have correct edges 5" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(conflict2, Causal, movement)) should be (successful)
    }
  }

  { // Paragraph 8
    val text = """
      |Urgent action to end the conflict, improve humanitarian access
      |to severely food insecure populations, and increase size and
      |scope of emergency assistance delivery is critical to save
      |lives over the coming year.
      """

    val action = NodeSpec("Urgent action to end the conflict")
    val access = NodeSpec("humanitarian access to severely food insecure populations", Inc("improve"))
    val assistance = NodeSpec("size and scope of emergency assitance", Inc("increase"))
    val lives = NodeSpec("lives", Inc("save"))

    behavior of "TestDoc5 Paragraph 8"

    val tester = new Tester(text)

    failingTest should "have correct edges 1" taggedAs(Mihai) in {
      tester.test(EdgeSpec(action, Causal, lives)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Mihai) in {
      tester.test(EdgeSpec(access, Causal, lives)) should be (successful)
    }
    failingTest should "have correct edges 3" taggedAs(Mihai) in {
      tester.test(EdgeSpec(assistance, Causal, lives)) should be (successful)
    }
  }

}
