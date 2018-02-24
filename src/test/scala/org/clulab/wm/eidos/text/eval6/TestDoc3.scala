package org.clulab.wm.eidos.text.eval6

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text._

class TestDoc3 extends Test {
  // EA_Seasonal_Monitor_2017_08_11 (AJAY)
  { // Paragraph 1 (Highlighted)
    val text = """
        |Rainfall in July continued to be above average over areas of eastern Sudan,
        |western Ethiopia, and northeastern South Sudan, which has been favorable for
        |cropping activities. In addition, heavier than normal rainfall is increasing
        |the risk of flooding in many of the flood-prone areas.
        """

    val tester = new Tester(text)

    val rainfall = NodeSpec("Rainfall", Quant("above average"))
    val cropActivity = NodeSpec("cropping activities")
    val rainfall2 = NodeSpec("rainfall", Quant("heavier than normal"))
    val floodRisk = NodeSpec("risk of flooding", Inc("increasing"))
    
    behavior of "TestDoc3 Paragraph 1"

    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall, Affect, cropActivity)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall2, Causal, floodRisk)) should be (successful) // Test edges connecting them
    }
  }

  {
    // Paragraph 2 (Highlighted)
    // Note: Has a causal link across sentences, but not creating a test for this as we only handle events within a sentence. todo: Is this correct ?
    val text = """
        |During the past month, rainfall was persistently heavy and well above average
        |over eastern Sudan, western Ethiopia, and northeastern South Sudan, with positive
        |anomalies of between 100 - 300 mm during this period. This has resulted in favorable
        |agricultural conditions, but also the potential for flooding in flood prone regions
        |of western and eastern lowlands of Sudan. Average to above-average rainfall has
        |extended into Yemen, southern Eritrea, northern South Sudan, Uganda, and western
        |Kenya (Figure 1). However, rainfall was below average in Afar and parts of Tigary
        |regions of northern Ethiopia, as well as in parts of southwestern South Sudan and
        |northern Uganda. The eastern Horn remained seasonally sunny and dry.
        """

    val tester = new Tester(text)

    val rainfall = NodeSpec("rainfall", Quant("persistently heavy", "well above average"))
    val agrConditions = NodeSpec("agricultural conditions", Unmarked("favorable"))
    val flooding = NodeSpec("flooding", Unmarked("potential"))
    val rainfall2 = NodeSpec("rainfall", Quant("Average", "above average"))
    val rainfall3 = NodeSpec("rainfall", Quant("below average"))
    val sunny = NodeSpec("sunny")
    val dry = NodeSpec("dry")

    behavior of "TestDoc3 Paragraph 2"

    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(rainfall)
    }
    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(agrConditions)
    }
    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(flooding)
    }
    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(rainfall2)
    }
    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(rainfall3)
    }
    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(sunny)
    }
    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(dry)
    }
  }

  {
    // Paragraph 3 (Highlighted)
    val text = """
        |Vegetatoin conditions remain above average in many northern areas of East Africa,
        |according to eMODIS/NDVI (Figure 2), likely as a result of ongoing above-average
        |rainfall in Sudan, South Sudan, and parts of western and central Ethiopia, as well
        |as eastern Tanzania. Meanwhile, vegetation conditions remain below average in much
        |of the eastern Horn, western and central Tanzania, southern Uganda, and eastern
        |Equatoria in South Sudan.
        """
    val tester = new Tester(text)

    val vegetation = NodeSpec("Vegetatoin conditions", Quant("above average")) //todo: Should we correct the typo here ??
    val rainfall = NodeSpec("rainfall", Quant("above-average"))
    val vegetation2 = NodeSpec("vegetation conditions", Quant("below average"))

    behavior of "TestDoc3 Paragraph 3"

    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall, Causal, vegetation)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(vegetation2)
    }

  }

  {
    // Paragraph 4 (Highlighted)
    val text ="""
        |In South Sudan, rainfall in July has generally been average to above average across
        |the country. Widespread rains have been favorable for crop development in Greater
        |Bahr el Ghazal and Greater Upper Nile states. According to the latest field reports,
        |there was a slight reduction in rainfall in July compared to the previous month,
        |over Wau and Jur River in Western Bhar el Gazal, Gogrial East, Gogrial West, and
        |Tonj East in Warrap, and Aweil Centre County in Northern Bhar el Gazal, but the
        |decline in rainfall was not enough to result in moisture stress on crops. Meanwhile,
        |in Greater Equatoria, favorable rainfall since the start of July has enabled agricultural
        |households to begin sowing. In parts of Central Equatoria (Juba County), Eastern Equatoria
        |(Magwi and Torit counties), Jonglei (Pibor and Pochalla counties), and Western Bahr el
        |Ghazal (Wau County), FAO and the National Ministry of Agriculture and Food Security have
        |confirm significant infestations of Fall Armyworm.
        """

    val tester = new Tester(text)

    val rainfall1 = NodeSpec("rainfall", Quant("average", "above average"))
    val rainfall2 = NodeSpec("rains", Quant("widespread"))
    val cropDevelopment = NodeSpec("crop development")
    val rainfall3 = NodeSpec("rainfall", Dec("reduction"))
    val rainfall4 = NodeSpec("rainfall", Dec("decline"))
    val moistureStress = NodeSpec("moisture stress on crops")
    val rainfall5 = NodeSpec("rainfall", Quant("favorable"))
    val sowing = NodeSpec("sowing")
    val infestation = NodeSpec("infestation of Fall Armyworm", Quant("significant"))

    behavior of "TestDoc3 Paragraph 4"

    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(rainfall1)
    }
    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall2, Affect, cropDevelopment)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(rainfall3)
    }
    failingTest should "have correct edges 1" taggedAs(Somebody) in { //todo: It is a NoCausal link .. should we differentiate .. how ?
      tester.test(EdgeSpec(rainfall4, Causal, moistureStress)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 1" taggedAs(Somebody) in { //todo: should there be a new type of edge to denote favorable ??
      tester.test(EdgeSpec(rainfall5, Affect, sowing)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(infestation)
    }


  }

  {
    // Paragraph 5 (Highlighted)
    val text ="""
        |Moderate to heavy rainfall is expected to continue during the next two weeks across
        |Sudan, South Suda, Uganda, northern Kenya, and western/northern Ethiopia (Figure 3).
        |Persistent heavy rains in the Ethiopian highlands, eastern Sudan, and South Sudan,
        |coupled with highly saturated soils in these areas, is likely to cause flooding in
        |flood-prone areas, as is typical in August.
        """

    val tester = new Tester(text)

    val rainfall = NodeSpec("rainfall", Quant("Moderate", "heavy"))
    val rainfall2 = NodeSpec("rains", Quant("Persistent", "heavy"))
    val soils = NodeSpec("soils", Quant("saturated", "highly"))
    val flood = NodeSpec("flooding")

    behavior of "TestDoc3 Paragraph 5"

    failingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(rainfall)
    }
    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall2, Causal, flood)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(soils, Causal, flood)) should be (successful) // Test edges connecting them
    }

  }

}
