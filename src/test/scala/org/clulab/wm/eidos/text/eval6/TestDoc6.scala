package org.clulab.wm.eidos.text.eval6

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text._

class TestDoc6 extends Test {
  // FEWSNET South Sudan Outlook January 2018 BG.pdf
  { // Paragraph 1
    val text = """
     |Crisis (IPC Phase 3) and Emergency (IPC Phase 4) outcomes persist in all regions of
     |South Sudan in January. It is expected most households who harvested have
     |depleted their stocks this month, three months earlier than was typical in pre-crisis
     |years. As a result, the 2018 lean season has started earlier than normal and food
     |security is expected to further deteriorate through the peak of the lean season in
     |July/August. Given expected very low food access during this time, there remains a
     |risk of Famine (IPC Phase 5) in a worst-case scenario of an extended absence of
     |assistance. Urgent action is needed to end the ongoing conflict and allow for
     |unhindered delivery of humanitarian assistance.
      """

    val tester = new Tester(text)

    val stocks = NodeSpec("stocks", Dec("depleted"))
    val leanSeason = NodeSpec("lean season", Unmarked("started earlier"))
    val foodSecurity = NodeSpec("food security", Dec("deteriorate"))
    val foodAccess = NodeSpec("food access", Dec("low", "very"))
    val risk = NodeSpec("risk of Famine")
    val assistance = NodeSpec("assistance", Dec("absence"))
    val action = NodeSpec("urgent action")
    val conflict = NodeSpec("ongoing conflict", Dec("end"))
    val assistance1 = NodeSpec("unhindered delivery of humanitarian assistance")
    
    behavior of "TestDoc6 Paragraph 1"

    // Requires corss-sentence coref
    futureWorkTest should "have correct edge 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(stocks, Causal, leanSeason)) should be (successful)
    }
    // Requires corss-sentence coref
    futureWorkTest should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(stocks, Causal, foodSecurity)) should be (successful)
    }
    failingTest should "have correct edges 3" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(foodAccess, Causal, risk)) should be (successful)
    }
    // Is this get-able?
    failingTest should "have correct edges 4" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(risk, Correlation, assistance)) should be (successful)
    }
    // These two should prob really be an EdgeSpec(conflict, Correlation, assistance1) -- 
    // todo: I think we could get this as [if X -> A and X -> B (within a single sentence), then A Corr B]
    // todo: as is, we'll get this as 2 causal events and "action" is likely a stop word here...
    failingTest should "have correct edges 5" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(action, Causal, conflict)) should be (successful)
    }
    failingTest should "have correct edges 6" taggedAs(Adarsh) in {
      tester.test(EdgeSpec(action, Causal, assistance1)) should be (successful)
    }

  }

  { // Paragraph 2
    val text = """
     |There remains high concern for households in Greater Baggari, though it is expected
     |many households in this area had access to some harvests and food security has
     |marginally improved. However, it is expected some households in this area lack
     |access to sufficient food and could be in Catastrophe (IPC Phase 5)*. Catastrophe
     |(IPC Phase 5) also remains likely in parts of Nyirol, Leer, and Ayod, where some
     |households did not harvest and ongoing insecurity is limiting access to assistance
     |and movement towards natural food sources.
     """

    val concerns = NodeSpec("concern", Inc("high"))
    val foodSec = NodeSpec("food security", Inc("improved", "marginally"))
    val access = NodeSpec("access to sufficient food", Dec("lack"))
    val catastrophe = NodeSpec("Catastrophe", Quant("likely"))
    val households = NodeSpec("households did not harvest") // todo -- handle
    val insecurity = NodeSpec("ongoing insecurity")
    val assistance = NodeSpec("access to assistance", Dec("limiting"))
    val movement = NodeSpec("movement towards natural food sources", Dec("limiting"))


    val tester = new Tester(text)

    behavior of "TestDoc6 Paragraph 2"

    failingTest should "have correct singleton node 1" taggedAs(Becky) in {
      tester.test(concerns) should be (successful)
    }
    failingTest should "have correct singleton node 2" taggedAs(Becky) in {
      tester.test(foodSec) should be (successful)
    }
    passingTest should "have correct singleton node 3" taggedAs(Becky) in {
      tester.test(access) should be (successful)
    }
    failingTest should "have correct edge 1" taggedAs(Becky) in {
      tester.test(EdgeSpec(catastrophe, Correlation, households)) should be (successful)
    }
    failingTest should "have correct edge 2" taggedAs(Becky) in {
      tester.test(EdgeSpec(insecurity, Causal, assistance)) should be (successful)
    }
    failingTest should "have correct edge 3" taggedAs(Becky) in {
      tester.test(EdgeSpec(insecurity, Causal, movement)) should be (successful)
    }

  }

  { // Paragraph 3
    val text = """
     |Despite the Cessation of Hostilities (COH) agreement signed in late December, armed
     |clashes between Government forces and armed opposition continue in Unity, Central
     |Equatoria, and parts of Western Equatoria. Additionally, tension remains high in parts
     |of Jonglei, Lakes, and Warrap, where inter-communal clashes have occurred, and in
     |central and southern Unity, due to the resurgence of various armed groups and an
     |uptick in cattle raiding. Despite ongoing conflict in many areas, 250 IDPs from Bor
     |PoC voluntarily returned to Fangak, and humanitarian partners supported the
     |relocation of the remaining 562 IDPs in Melut PoC prior to the closure of the camp.
     """

    val tension = NodeSpec("tension", Inc("high"))
    val clashes = NodeSpec("inter-communal clashes have occurred")
    val armedGroups = NodeSpec("various armed groups", Inc("resurgence"))
    val cattleRaiding = NodeSpec("cattle raiding", Inc("uptick"))

    val tester = new Tester(text)

    behavior of "TestDoc6 Paragraph 3"

    failingTest should "have correct edges 1" taggedAs(Becky) in {
      tester.test(EdgeSpec(tension, Correlation, clashes)) should be (successful)
    }
    failingTest should "have correct edges 2" taggedAs(Becky) in {
      tester.test(EdgeSpec(armedGroups, Causal, tension)) should be (successful)
    }
    failingTest should "have correct edges 3" taggedAs(Becky) in {
      tester.test(EdgeSpec(cattleRaiding, Causal, tension)) should be (successful)
    }
  }

  { // Paragraph 4
    val text = """
     |Data collection for the Food Security and Nutrition Monitoring System (FSNMS) has
     |been completed in most counties of South Sudan. These data, collected in
     |December and January, provide information on likely food security and nutrition
     |outcomes during the post-harvest period. The South Sudan Technical Working Group
     |will meet in late January/early February and utilize these data, and other recent
     |assessments, to conduct an IPC analysis. This analysis will analyse current food
     |security outcomes and project food security outcomes through July 2018.
     """

    val concerns = NodeSpec("Concerns about insufficient food access")

    behavior of "TestDoc6 Paragraph 4"

  }

  { // Paragraph 5
    val text = """
     |According to the IPC, a Famine (IPC Phase 5) has occurred when at least 20 percent
     |of households in a given area have an extreme lack of food, the Global Acute
     |Malnutrition (GAM) prevalence, as measured by weight-for-height z-score (WHZ),
     |exceeds 30 percent, and mortality, as measured by the Crude Death Rate (CDR), is
     |greater than 2 per 10,000 per day. Catastrophe (IPC Phase 5) is when a household
     |group has an extreme lack of food and/or other basic needs even with full employment
     |of coping strategies.
     """

    // Todo: should we get all these measures of Famine as Correlations?
    // todo: should we have an inferred (neg) correlation with "employment of coping strategies"?
    val food = NodeSpec("food", Dec("lack", "extreme"))
    val needs = NodeSpec("basic needs", Dec("lack", "extreme"))

    val tester = new Tester(text)

    behavior of "TestDoc6 Paragraph 5"

    failingTest should "have correct singleton node 1" taggedAs(Mihai) in {
      tester.test(food) should be (successful)
    }
    failingTest should "have correct singleton node 2" taggedAs(Mihai) in {
      tester.test(needs) should be (successful)
    }
  }
}
