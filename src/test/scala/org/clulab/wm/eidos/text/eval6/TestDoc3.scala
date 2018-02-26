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

    failingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall, Affect, cropActivity)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 2" taggedAs(Ajay) in {
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

    failingTest should "have correct singleton node 1" taggedAs(Ajay) in {
      tester.test(rainfall)
    }
    failingTest should "have correct singleton node 2" taggedAs(Ajay) in {
      tester.test(agrConditions)
    }
    failingTest should "have correct singleton node 3" taggedAs(Ajay) in {
      tester.test(flooding)
    }
    failingTest should "have correct singleton node 4" taggedAs(Ajay) in {
      tester.test(rainfall2)
    }
    failingTest should "have correct singleton node 5" taggedAs(Ajay) in {
      tester.test(rainfall3)
    }
    failingTest should "have correct singleton node 6" taggedAs(Ajay) in {
      tester.test(sunny)
    }
    failingTest should "have correct singleton node 7" taggedAs(Ajay) in {
      tester.test(dry)
    }
  }

  {
    // Paragraph 3 (Highlighted)
    // Note: typo 'Vegetatoin' corrected
    val text = """
        |Vegetation conditions remain above average in many northern areas of East Africa,
        |according to eMODIS/NDVI (Figure 2), likely as a result of ongoing above-average
        |rainfall in Sudan, South Sudan, and parts of western and central Ethiopia, as well
        |as eastern Tanzania. Meanwhile, vegetation conditions remain below average in much
        |of the eastern Horn, western and central Tanzania, southern Uganda, and eastern
        |Equatoria in South Sudan.
        """
    val tester = new Tester(text)

    val vegetation = NodeSpec("Vegetation conditions", Quant("above average"))
    val rainfall = NodeSpec("rainfall", Quant("above-average"))
    val vegetation2 = NodeSpec("vegetation conditions", Quant("below average"))

    behavior of "TestDoc3 Paragraph 3"

    failingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall, Causal, vegetation)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct singleton node 1" taggedAs(Ajay) in {
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

    failingTest should "have correct singleton node 1" taggedAs(Ajay) in {
      tester.test(rainfall1)
    }
    failingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall2, Affect, cropDevelopment)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct singleton node 2" taggedAs(Ajay) in {
      tester.test(rainfall3)
    }
    failingTest should "have correct edges 2" taggedAs(Ajay) in { //todo: It is a NoCausal link .. should we differentiate .. how ?
      tester.test(EdgeSpec(rainfall4, Causal, moistureStress)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 3" taggedAs(Ajay) in { //todo: should there be a new type of edge to denote favorable ??
      tester.test(EdgeSpec(rainfall5, Affect, sowing)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct singleton node 3" taggedAs(Ajay) in {
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

    failingTest should "have correct singleton node 1" taggedAs(Ajay) in {
      tester.test(rainfall)
    }
    failingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall2, Causal, flood)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 2" taggedAs(Ajay) in {
      tester.test(EdgeSpec(soils, Causal, flood)) should be (successful) // Test edges connecting them
    }
  }

  // Following are the paragraphs which are not highlighted but are included to make the system more robust
  {
    // Paragraph 6
    val text ="""
        |Rainfall deficits have accumulated over the past 30 days in Tigray and northern Afar Regions
        |of Ethiopia. In Kenya, a late start to the season and extended dry spells have led to reduced
        |crop yields and poorer than usual crop production prospects.
        """

    val tester = new Tester(text)

    // Nodes here
    val rainfall = NodeSpec("Rainfall", Dec("deficits"))
    val season = NodeSpec("season", Quant("late start"))
    val drySpell = NodeSpec("dry spells", Inc("extended"))
    val cropYield = NodeSpec("crop yields", Dec("reduced"))
    val cropProd = NodeSpec("crop production prospects", Dec("poorer than usual"))

    behavior of "TestDoc3 Paragraph 6"

    // tests here
    failingTest should "have correct singleton node 1" taggedAs(Ajay) in {
      tester.test(rainfall)
    }
    failingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(season, Causal, cropYield)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 2" taggedAs(Ajay) in {
      tester.test(EdgeSpec(season, Causal, cropProd)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 3" taggedAs(Ajay) in {
      tester.test(EdgeSpec(drySpell, Causal, cropYield)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 4" taggedAs(Ajay) in {
      tester.test(EdgeSpec(drySpell, Causal, cropProd)) should be (successful) // Test edges connecting them
    }
  }

  {
    // Paragraph 7
    val text ="""
        |During the next two weeks, moderate to heavy seasonal rainfall is expected to continue over
        |western Ethiopia and eastern Sudan lowlands, with heightened risk of flooding in these regions.Meanwhile,
        |above-average rainfall is forecast over northern Ethiopia, which could help reduce recent rainfall deficits.
        """

    val tester = new Tester(text)

    // Nodes here
    val rainfall = NodeSpec("seasonal rainfall", Quant("moderate", "heavy"))
    val flooding = NodeSpec("risk of flooding", Inc("heightened"))
    val rainfall2 = NodeSpec("rainfall", Quant("above-average"))
    val rainfallDeficit = NodeSpec("rainfall deficit", Dec("reduce"))

    behavior of "TestDoc3 Paragraph 7"

    // tests here
    failingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall, Correlation, flooding)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 2" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall2, Causal, rainfallDeficit)) should be (successful) // Test edges connecting them
    }
  }

  {
    // Paragraph 8
    val text ="""
        |Satellite-derived estimates suggest water resources are continuing to decline at surface water points in
        |the Mandera triangle, which include the predominantly pastoral areas of eastern and southern Ethiopia,
        |northeastern Kenya, and southern Somalia. The current dry and abnormally hotter-than-normal
        |land-surface-temperatures are expected to continue until the Deyr/Hageya/short rains season, which
        |normally starts in October.
        """

    val tester = new Tester(text)

    // Nodes here
    val water = NodeSpec("water resources", Dec("decline"))
    val landTemp = NodeSpec("land-surface-temperatures", Quant("dry", "abnormally hotter-than-normal"))

    behavior of "TestDoc3 Paragraph 8"

    // tests here
    failingTest should "have correct singleton node 1" taggedAs(Ajay) in {
      tester.test(water)
    }
    failingTest should "have correct singleton node 2" taggedAs(Ajay) in {
      tester.test(landTemp)
    }
  }

  {
    // Paragraph 9
    val text ="""
        |Cropping conditions are generally favorable in most western areas and central areas due to good
        |performance of seasonal rainfall since the start of the season. However, persistently well above-average
        |rainfall over thewestern Ethiopia highlands could result in flooding during the coming weeks. These
        |continued rains could also reduce the impact of Fall Armyworm, for which infestations have been reported
        |in six regions of Ethiopia. Rainfall is forecast to be moderate to heavy in the coming weeks, which will
        |likely contribute to the potential for flooding and help erase rainfall deficits in some northern areas.
        """

    val tester = new Tester(text)

    // Nodes here

    behavior of "TestDoc3 Paragraph 9"
    val cropCond = NodeSpec("Cropping conditions", Quant("favorable"))
    val rainfall = NodeSpec("seasonal rainfall")
    val rainfall2 = NodeSpec("rainfall", Quant("above-average"))
    val flood = NodeSpec("flooding")
    val rainfall3 = NodeSpec("rains")
    val worm = NodeSpec("impact of Fall Armyworm", Dec("reduce")) //todo: Is it "impact of Fall Armyworm" or just "Fall Armyworm" ??
    val rainfall4 = NodeSpec("rainfall", Quant("moderate", "heavy"))
    val flood2 = NodeSpec("flooding")
    val rainfallDeficit = NodeSpec("rainfall deficits", Dec("erase")) //todo: Is it "rainfall deficits" or "rainfall Dec("deficits") " ??

    // tests here
    failingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall, Causal, cropCond)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 2" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall2, Causal, flood)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 3" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall3, Causal, worm)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 4" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall4, Causal, flood2)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 5" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall4, Causal, rainfallDeficit)) should be (successful) // Test edges connecting them
    }
  }

  {
    // Paragraph 10
    val text ="""
        |Somalia remained seasonally sunny and dry, except in northern coastal areas of Somalia and areas bordering
        |eastern Djibouti, where Karan/Xagaa seasonal rains were generally light and below average. Pasture and water
        |resources have continued to decline in much of central and northern Somalia. However, southern regions of
        |Somalia have benefited from localized coastal rains for the past month.
        """

    val tester = new Tester(text)

    // Nodes here
    val sunny = NodeSpec("sunny")
    val dry = NodeSpec("dry")
    val rainfall = NodeSpec("seasonal rains", Quant("light", "below average"))
    val pasture = NodeSpec("Pasture", Dec("decline"))
    val water = NodeSpec("water resources", Dec("decline"))
    val rainfall2 = NodeSpec("coastal rains")

    behavior of "TestDoc3 Paragraph 10"

    // tests here
    failingTest should "have correct singleton node 1" taggedAs(Ajay) in {
      tester.test(sunny)
    }
    failingTest should "have correct singleton node 2" taggedAs(Ajay) in {
      tester.test(dry)
    }
    failingTest should "have correct singleton node 3" taggedAs(Ajay) in {
      tester.test(rainfall)
    }
    failingTest should "have correct singleton node 4" taggedAs(Ajay) in {
      tester.test(pasture)
    }
    failingTest should "have correct singleton node 5" taggedAs(Ajay) in {
      tester.test(water)
    }
    failingTest should "have correct singleton node 6" taggedAs(Ajay) in {
      tester.test(rainfall2)
    }
  }

  {
    // Paragraph 11
    // Note: typo "Key agricultural areas areas.." corrected
    val text ="""
        |In Kenya, the shortened length of the main growing season, due in part to a delayed onset of seasonal
        |rainfall, coupled with long dry spells and below-average rainfall is resulting in below-average production
        |prospects in large parts of the eastern, central, and southern Rift Valley. Key agricultural production
        |areas were also affected by an erratic onset of rainfall, prolonged dry spells in June, and FAW,
        |especially in Uasin-Gishu and parts of Trans-Nzoia counties, where maize yields are expected
        |to be particularly poor.
        """

    val tester = new Tester(text)

    // Nodes here
    val growSeason = NodeSpec("main growing season", Dec("shortened"))
    val rainfall = NodeSpec("onset of seasonal rainfall", Quant("delayed"))
    val drySpell = NodeSpec("dry spells", Quant("long"))
    val rainfall2 = NodeSpec("rainfall", Quant("below-average"))
    val production = NodeSpec("production prospects", Quant("below-average"))
    val prodArea = NodeSpec("Key agricultural production areas")
    val rainfall3 = NodeSpec("rainfall", Quant("erratic"))
    val drySpell2 = NodeSpec("dry spells", Quant("prolonged"))
    val maize = NodeSpec("maize yields", Quant("poor"))

    behavior of "TestDoc3 Paragraph 11"

    // tests here
    failingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall, Causal, growSeason)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 2" taggedAs(Ajay) in {
      tester.test(EdgeSpec(drySpell, Causal, production)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 3" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall2, Causal, production)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 4" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall3, Affect, prodArea)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 5" taggedAs(Ajay) in {
      tester.test(EdgeSpec(drySpell2, Affect, prodArea)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct singleton node 1" taggedAs(Ajay) in {
      tester.test(maize)
    }
  }

  {
    // Paragraph 12
    val text =
      """
        |In Karamoja in Uganda, rainfall during the past month has been average to above average, an improvement
        |over below-average rainfall received early in the rainy season. This, coupled with a continuation of
        |above-average rainfall forecast in the coming weeks, is expected to improve crop production prospects and
        |pasture conditions. In areas of northern Uganda, particularly West Nile Region where planting was
        |significantly delayed by almost a month, late-planted crops are currently in good condition and in
        |maturity stages of grain filling. Harvesting and drying of first season cereals and legumes is ongoing
        |in most bi-modal areas of central, eastern, south and western Uganda. However, the forecast widespread
        |rainfall during the next 1-2 weeks may delay harvesting and drying activities.
        """

    val tester = new Tester(text)

    // Nodes here
    val rainfall = NodeSpec("rainfall", Quant("average", "above average"))
    val rainfall2 = NodeSpec("rainfall", Quant("below-average"))
    val rainfall3 = NodeSpec("rainfall", Quant("above-average")) //todo: Should we differentiate between rainfall and rainfall forecast ??
    //todo: Should we do coref resolve of "This, ..."
    val cropProd = NodeSpec("crop production prospects", Inc("improve"))
    val pastures = NodeSpec("pasture conditions", Inc("improve"))
    val planting = NodeSpec("planting", Quant("significantly", "delayed"))
    val crops = NodeSpec("crops", Quant("late-planted", "good condition", "maturity stages of grain filling")) //todo: Is this right ??
    val cereal = NodeSpec("cereals")
    val legumes = NodeSpec("legumes")
    val harvest = NodeSpec("Harvesting")
    val drying = NodeSpec("drying")
    val rainfall4 = NodeSpec("rainfall", Quant("widespread")) //todo: rainfall vs. rainfall forecast
    val harvest2 = NodeSpec("harvesting")
    val drying2 = NodeSpec("drying")

    behavior of "TestDoc3 Paragraph 12"

    // tests here
    failingTest should "have correct singleton node 1" taggedAs(Ajay) in {
      tester.test(rainfall)
    }
    failingTest should "have correct singleton node 2" taggedAs(Ajay) in {
      tester.test(rainfall2)
    }
    failingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall3, Causal, cropProd)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 2" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall3, Causal, pastures)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct singleton node 3" taggedAs(Ajay) in {
      tester.test(planting)
    }
    failingTest should "have correct singleton node 4" taggedAs(Ajay) in {
      tester.test(crops)
    }
    failingTest should "have correct singleton node 5" taggedAs(Ajay) in {
      tester.test(harvest)
    }
    failingTest should "have correct singleton node 6" taggedAs(Ajay) in {
      tester.test(drying)
    }
    failingTest should "have correct singleton node 7" taggedAs(Ajay) in {
      tester.test(cereal)
    }
    failingTest should "have correct singleton node 8" taggedAs(Ajay) in {
      tester.test(legumes)
    }
    failingTest should "have correct singleton node 9" taggedAs(Ajay) in {
      tester.test(rainfall4)
    }
    failingTest should "have correct singleton node 10" taggedAs(Ajay) in {
      tester.test(harvest2)
    }
    failingTest should "have correct singleton node 11" taggedAs(Ajay) in {
      tester.test(drying2)
    }
  }

  {
    // Paragraph 13
    val text ="""
        |In Sudan, seasonal rainfall has continued to be above average during the past few weeks, which has been
        |favorable in most cropping areas of the country. The weekly forecasts indicate continued intensification
        |of seasonal rainfall, with increased likelihood for flooding in flood-prone aras of eastern Sudan.
        |However, there are localized areas in western Darfur where July rains were below average, resulting in
        |drier-than-normal vegetation conditions. In the coming weeks, the seasonal rains are expected to intensify
        |and may help ease the current dry conditions in parts of western Darfur.
        """

    val tester = new Tester(text)

    // Nodes here
    val rainfall = NodeSpec("seasonal rainfall", Quant("above average"))
    val crop = NodeSpec("cropping areas")
    val rainfall2 = NodeSpec("seasonal rainfall", Quant("intensification"))
    val flooding = NodeSpec("flooding", Inc("increased"))
    val rainfall3 = NodeSpec("rains", Quant("below average"))
    val vegetation = NodeSpec("vegetation conditions", Quant("drier-than=normal"))
    val rainfall4 = NodeSpec("seasonal rains", Quant("intensify"))
    val dry = NodeSpec("dry conditions", Quant("ease")) //todo: Is it a quant ??

    behavior of "TestDoc3 Paragraph 13"

    // tests here
    failingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall, Affect, crop)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 2" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall2, Correlation, flooding)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 3" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall3, Causal, vegetation)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 4" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall4, Causal, dry)) should be (successful) // Test edges connecting them
    }
  }

  {
    // Paragraph 14
    val text ="""
        |In Yemen, second season (July to September) rainfall season started on time in July and has been above
        |average across western, central, and parts of eastern Yemen. Vegetation conditions are currently below
        |across many western coastal and mountainous areas of the country, with above-average vegetation conditions
        |in parts of Ta'izz and Ibb governorates. Although, the short-and long-term rainfall forecasts are favorable
        |for agricultural areas of the western and central highlands, production of most crops is likely to be
        |limited due to insecurity and lack of availability and/or access to farm inputs due to ongoing conflict.
        """

    val tester = new Tester(text)

    // Nodes here
    val rainfall = NodeSpec("rainfall season", Quant("above average"))
    val vegetation = NodeSpec("Vegetation conditions", Dec("below"))
    val vegetation2 = NodeSpec("vegetation", Quant("above-average"))
    val rainfall2 = NodeSpec("rainfall forecasts", Quant("favorable"))
    val production = NodeSpec("production of crops", Quant("limited"))
    val insecurity = NodeSpec("insecurity")
    val farmOutput = NodeSpec("farm outputs", Dec("lack"))
    val farmInput = NodeSpec("farm input", Quant("access"))
    val conflict = NodeSpec("conflict")

    behavior of "TestDoc3 Paragraph 14"

    // tests here
    failingTest should "have correct singleton node 1" taggedAs(Ajay) in {
      tester.test(rainfall)
    }
    failingTest should "have correct singleton node 2" taggedAs(Ajay) in {
      tester.test(vegetation)
    }
    failingTest should "have correct singleton node 3" taggedAs(Ajay) in {
      tester.test(vegetation2)
    }
    failingTest should "have correct singleton node 4" taggedAs(Ajay) in {
      tester.test(rainfall2)
    }
    failingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(insecurity, Causal, production)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 2" taggedAs(Ajay) in {
      tester.test(EdgeSpec(farmOutput, Causal, production)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 3" taggedAs(Ajay) in {
      tester.test(EdgeSpec(farmInput, Causal, production)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 4" taggedAs(Ajay) in {
      tester.test(EdgeSpec(conflict, Causal, farmInput)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 5" taggedAs(Ajay) in {
      tester.test(EdgeSpec(conflict, Causal, farmOutput)) should be (successful) // Test edges connecting them
    }
  }

  {
    // Paragraph 15
    val text ="""
        |The expected intensification of seasonal rains over northern Uganda is expected to help gradually ease
        |rainfall deficits that developed earlier in the season. In parts of western Kenya and central/eastern
        |Uganda, the expected heavy rains may hamper ongoing maize harvesting and drying activities and could
        |result in post-harvest losses, if the rains are sustained without significant dry days in coming weeks.
        |Similarly for eastern and central Uganda, where crop harvesting is also currently on-going.
        """

    val tester = new Tester(text)

    // Nodes here
    val rainfall = NodeSpec("seasonal rains", Quant("intensification"))
    val rainfallDeficit = NodeSpec("rainfall deficits", Quant("ease"))
    val rainfall2 = NodeSpec("rain", Quant("heavy"))
    val maize = NodeSpec("maize harvesting", Dec("hamper"))
    val drying = NodeSpec("drying activities", Dec("hamper"))
    val losses = NodeSpec("post-harvest losses")
    val rainfall3 = NodeSpec("rains")
    val dry = NodeSpec("dry days", Dec("without"))
    val cropHarvest = NodeSpec("crop harvesting")

    behavior of "TestDoc3 Paragraph 15"

    // tests here
    failingTest should "have correct edges 1" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall, Causal, rainfallDeficit)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 2" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall2, Causal, maize)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 3" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall2, Causal, drying)) should be (successful) // Test edges connecting them
    }
    //todo: Check whether the following 2 causations are correct or should 'rain' be the cause ??
    failingTest should "have correct edges 4" taggedAs(Ajay) in {
      tester.test(EdgeSpec(maize, Causal, losses)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 5" taggedAs(Ajay) in {
      tester.test(EdgeSpec(drying, Causal, losses)) should be (successful) // Test edges connecting them
    }
    //todo: "if the rains are sustained without significant dry days in coming weeks." --> how to add tests for these ?
    failingTest should "have correct singleton node 1" taggedAs(Ajay) in {
      tester.test(rainfall3)
    }
    failingTest should "have correct singleton node 2" taggedAs(Ajay) in {
      tester.test(dry)
    }
    failingTest should "have correct singleton node 3" taggedAs(Ajay) in {
      tester.test(cropHarvest)
    }
  }

  {
    // Paragraph 16
    val text ="""
        |The coastal strip of Kenya and southern Somalia are also likely to receive light to moderate rains.
        |The rest of eastern Horn is forecast to remain generally sunny and dry, as is seasonally normal.
        """

    val tester = new Tester(text)

    // Nodes here
    val rainfall = NodeSpec("rains", Quant("light", "moderate"))
    val sunny = NodeSpec("sunny")
    val dry = NodeSpec("dry")

    behavior of "TestDoc3 Paragraph 16"

    // tests here
    failingTest should "have correct singleton node 1" taggedAs(Ajay) in {
      tester.test(rainfall)
    }
    failingTest should "have correct singleton node 2" taggedAs(Ajay) in {
      tester.test(sunny)
    }
    failingTest should "have correct singleton node 3" taggedAs(Ajay) in {
      tester.test(dry)
    }
  }

}
