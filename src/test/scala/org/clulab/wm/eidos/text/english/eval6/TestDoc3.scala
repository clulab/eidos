package org.clulab.wm.eidos.text.english.eval6

import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.test.TestUtils._

class TestDoc3 extends EnglishTest {
  // EA_Seasonal_Monitor_2017_08_11 (AJAY)
  { // Paragraph 1 (Highlighted)
    val text = """
        |Rainfall in July continued to be above average over areas of eastern Sudan,
        |western Ethiopia, and northeastern South Sudan, which has been favorable for
        |cropping activities. In addition, heavier than normal rainfall is increasing
        |the risk of flooding in many of the flood-prone areas.
        """

    val tester = new GraphTester(text)

    val rainfall = NodeSpec("Rainfall", Quant("average"), Inc("above average"))
    val cropActivity = NodeSpec("cropping activities")
    val rainfall2 = NodeSpec("normal rainfall", Quant("heavier than normal"), Inc("heavier than normal"))
    val floodRisk = NodeSpec("risk of flooding", Inc("increasing"))
    
    behavior of "TestDoc3 Paragraph 1"

    // Note: There is no good path here.
    // should we even get this??
    futureWorkTest should "have correct edges 1" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(rainfall, Causal, cropActivity)) should be (successful) // Test edges connecting them
    }
    passingTest should "have correct edges 2" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(rainfall2, Causal, floodRisk)) should be (successful) // Test edges connecting them
    }
  }

  {
    // Paragraph 2 (Highlighted)
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

    val tester = new GraphTester(text)

    // To get "well above" as a single quantifier perhaps add it to Quantifier.tsv ?
    val rainfall = NodeSpec("rainfall", Inc("above", "well"), Quant("heavy", "persistently"), Quant("average", "well above"))
    val agrConditions = NodeSpec("agricultural conditions", Quant("favorable"))
    val flooding = NodeSpec("potential for flooding")
    val rainfall2 = NodeSpec("rainfall", Quant("Average to above-average"))
    val rainfall3 = NodeSpec("rainfall", Dec("below"), Quant("below average"))

    behavior of "TestDoc3 Paragraph 2"

    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(rainfall)
    }
    passingTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(agrConditions)
    }
    // Note: Has a causal link across sentences, so a futureWorkTest.
    futureWorkTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall, Causal, agrConditions)) should be (successful) // Test edges connecting them
    }
    passingTest should "have correct singleton node 3" taggedAs(Somebody) in {
      tester.test(flooding)
    }
    passingTest should "have correct singleton node 4" taggedAs(Somebody) in {
      tester.test(rainfall2)
    }
    passingTest should "have correct singleton node 5" taggedAs(Somebody) in {
      tester.test(rainfall3)
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
    val tester = new GraphTester(text)

    // To get "above average" as a single quantifier perhaps add it to Quantifier.tsv ?
    val vegetation = NodeSpec("Vegetation conditions", Inc("above average"), Quant("average"))
    val rainfall = NodeSpec("rainfall", Inc("above-average"), Quant("above-average", "ongoing"))
    val vegetation2 = NodeSpec("vegetation conditions", Dec("below"), Quant("below average"))

    behavior of "TestDoc3 Paragraph 3"


    // Note: There is no good path here.
    futureWorkTest should "have correct edges 1" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(rainfall, Causal, vegetation)) should be (successful) // Test edges connecting them
    }
    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
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
        |confirmed significant infestations of Fall Armyworm.
        """

    val tester = new GraphTester(text)

    val rainfall1 = NodeSpec("rainfall", Quant("average to above average"))
    val rainfall2 = NodeSpec("Widespread rains", Inc("Widespread"), Inc("favorable"), Quant("favorable"))
    val cropDevelopment = NodeSpec("crop development in Greater Bahr el Ghazal and Greater Upper Nile states", GeoLoc("Greater Bahr"), GeoLoc("Ghazal"), GeoLoc("Greater Upper Nile"), Inc("favorable"))
    val rainfall3 = NodeSpec("rainfall", Dec("reduction", "slight")) // todo (temporal?): really should capture the "compared to the previous month"...
    val rainfall4 = NodeSpec("rainfall", Dec("decline"))
    val moistureStress = NodeSpec("moisture stress on crops")
    val rainfall5 = NodeSpec("Meanwhile, in Greater Equatoria, favorable rainfall", GeoLoc("Greater Equatoria"), Inc("favorable"), Quant("favorable"))
    val sowing = NodeSpec("agricultural households to begin sowing")
    val infestation = NodeSpec("infestations of Fall Armyworm", Quant("significant"))

    behavior of "TestDoc3 Paragraph 4"

    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(rainfall1)
    }
    passingTest should "have correct edges 1" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(rainfall2, Causal, cropDevelopment)) should be (successful)
    }
    passingTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(rainfall3)
    }
    futureWorkTest should "have correct edges 2" taggedAs(Somebody) in { //Note: Adding a causal link here. todo: But the issue of noCausal is still present in the system. will be addressed later
      tester.test(EdgeSpec(rainfall4, Causal, moistureStress)) should be (successful) // Test edges connecting them
    }
    passingTest should "have correct edges 3" taggedAs(Egoitz) in {
      tester.test(EdgeSpec(rainfall5, Causal, sowing)) should be (successful) // Test edges connecting them
    }
    // Removing for now bc we're only expanding entities involved in Causal relations
//    passingTest should "have correct singleton node 3" taggedAs(Egoitz) in {
//      tester.test(infestation)
//    }
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

    val tester = new GraphTester(text)

    // To get "q1 to q2" as a quantifier, add rule to entityQuantification.yml
    val rainfall = NodeSpec("rainfall", Quant("Moderate to heavy"))
    val rainfall2 = NodeSpec("rains in the Ethiopian highlands", Quant("Persistent heavy"))
    val soils = NodeSpec("soils", Quant("saturated", "highly"))
    val flood = NodeSpec("flooding in flood-prone areas")

    behavior of "TestDoc3 Paragraph 5"

    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(rainfall)
    }
    // todo: when hyperedges added, these tests should prob be combined to make one with 2 causes
    failingTest should "have correct edges 1" taggedAs(Becky) in {
      tester.test(EdgeSpec(rainfall2, Causal, flood)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 2" taggedAs(Becky) in {
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

    val tester = new GraphTester(text)

    // Nodes here
    val rainfall = NodeSpec("Rainfall", Dec("deficits"))
    val season = NodeSpec("late start to the season")
    val drySpell = NodeSpec("extended dry spells")
    val cropYield = NodeSpec("reduced crop yields", Dec("reduced"))
    // todo: this Dec trigger will likely need a specialized rule ->  ${triggers} than /[usual/typical/average... etc]/
    val cropProd = NodeSpec("usual crop production prospects", Dec("poorer than usual"), Quant("usual"))

    behavior of "TestDoc3 Paragraph 6"

    // tests here
    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(rainfall)
    }
    passingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(season, Causal, cropYield)) should be (successful) // Test edges connecting them
    }
    passingTest should "have correct edges 2" taggedAs(Becky) in {
      tester.test(EdgeSpec(season, Causal, cropProd)) should be (successful) // Test edges connecting them
    }
    passingTest should "have correct edges 3" taggedAs(Becky) in {
      tester.test(EdgeSpec(drySpell, Causal, cropYield)) should be (successful) // Test edges connecting them
    }
    passingTest should "have correct edges 4" taggedAs(Becky) in {
      tester.test(EdgeSpec(drySpell, Causal, cropProd)) should be (successful) // Test edges connecting them
    }
  }

  {
    // Paragraph 7
    val text ="""
        |During the next two weeks, moderate to heavy seasonal rainfall is expected to continue over
        |western Ethiopia and eastern Sudan lowlands, with heightened risk of flooding in these regions. Meanwhile,
        |above-average rainfall is forecast over northern Ethiopia, which could help reduce recent rainfall deficits.
        """

    val tester = new GraphTester(text)

    // Nodes here
    val rainfall = NodeSpec("seasonal rainfall", Inc("heavy"), Quant("moderate to heavy"))
    val flooding = NodeSpec("risk of flooding in these regions", Inc("heightened"))
    val rainfall2 = NodeSpec("rainfall", Inc("above-average"), Quant("above-average"))
    val rainfallDeficit = NodeSpec("recent rainfall", Dec("deficits"), Dec("reduce"))

    behavior of "TestDoc3 Paragraph 7"

    // tests here
    // We removed the "with" rules bc they were very much over-firing
    futureWorkTest should "have correct edges 1" taggedAs(Zheng) in {
      tester.test(EdgeSpec(rainfall, Correlation, flooding)) should be (successful) // Test edges connecting them
    }
    // Dependency Path Error
    brokenSyntaxTest should "have correct edges 2" taggedAs(Zheng) in {
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

    val tester = new GraphTester(text)

    // Nodes here
    val water = NodeSpec("water resources", Dec("decline"))
    val landTemp = NodeSpec("land-surface-temperatures", Quant("dry"), Quant("hotter-than-normal", "abnormally"))

    behavior of "TestDoc3 Paragraph 8"

    // tests here
    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(water)
    }
    passingTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(landTemp)
    }
  }

  {
    // Paragraph 9
    val text ="""
        |Cropping conditions are generally favorable in most western areas and central areas due to good
        |performance of seasonal rainfall since the start of the season. However, persistently well above-average
        |rainfall over the western Ethiopia highlands could result in flooding during the coming weeks. These
        |continued rains could also reduce the impact of Fall Armyworm, for which infestations have been reported
        |in six regions of Ethiopia. Rainfall is forecast to be moderate to heavy in the coming weeks, which will
        |likely contribute to the potential for flooding and help erase rainfall deficits in some northern areas.
        """

    val tester = new GraphTester(text)

    // Nodes here

    behavior of "TestDoc3 Paragraph 9"
    val cropCond = NodeSpec("Cropping conditions", Inc("favorable"), Quant("favorable"))
    val rainfall = NodeSpec("good performance of seasonal rainfall", Quant("good"), Inc("good"))
    val rainfall2 = NodeSpec("persistently well above-average rainfall over the western Ethiopia highlands", Inc("above-average","persistently", "well"), Quant("above-average", "persistently", "well"), GeoLoc("Ethiopia"), TimEx("coming weeks"))
    val flood = NodeSpec("flooding", TimEx("coming weeks"))
    val rainfall3 = NodeSpec("continued rains")
    val worm = NodeSpec("impact of Fall Armyworm", Dec("reduce"))
    val rainfall4 = NodeSpec("Rainfall", Quant("moderate to heavy"), TimEx("coming weeks"))
    val flood2 = NodeSpec("potential for flooding")
    val rainfallDeficit = NodeSpec("rainfall deficits", Dec("deficits"), Dec("erase"))

    // tests here
    passingTest should "have correct edges 1" taggedAs(Zheng) in {
      tester.test(EdgeSpec(rainfall, Causal, cropCond)) should be (successful) // Test edges connecting them
    }
    passingTest should "have correct edges 2" taggedAs(Zheng) in {
      tester.test(EdgeSpec(rainfall2, Causal, flood)) should be (successful) // Test edges connecting them
    }
    passingTest should "have correct edges 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall3, Causal, worm)) should be (successful) // Test edges connecting them
    }
    passingTest should "have correct edges 4" taggedAs(Zheng) in {
      tester.test(EdgeSpec(rainfall4, Causal, flood2)) should be (successful) // Test edges connecting them
    }
    passingTest should "have correct edges 5" taggedAs(Zheng) in {
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

    val tester = new GraphTester(text)

    // Nodes here
    val sunny = NodeSpec("sunny")
    val dry = NodeSpec("dry")
    val rainfall = NodeSpec("seasonal rains", Dec("below"), Quant("light"), Quant("below average")) //Note: Looks like a conj .. so keeping 2 quants
    val pasture = NodeSpec("Pasture", Dec("decline"))
    val water = NodeSpec("water resources", Dec("decline"))

    behavior of "TestDoc3 Paragraph 10"

    // tests here
    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(sunny)
    }
    passingTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(dry)
    }
    passingTest should "have correct singleton node 3" taggedAs(Somebody) in {
      tester.test(rainfall)
    }
    passingTest should "have correct singleton node 4" taggedAs(Somebody) in {
      tester.test(pasture)
    }
    passingTest should "have correct singleton node 5" taggedAs(Somebody) in {
      tester.test(water)
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

    val tester = new GraphTester(text)

    // Nodes here
    val growSeason = NodeSpec("shortened length of the main growing season", Dec("shortened"))
    val rainfall = NodeSpec("delayed onset of seasonal rainfall", Quant("delayed"))
    val drySpell = NodeSpec("dry spells", Quant("long"))
    val rainfall2 = NodeSpec("rainfall", Dec("below-average"), Quant("below-average"))
    val production = NodeSpec("below-average production prospects in large parts of the eastern, central, and southern Rift Valley", Dec("below-average"), Quant("below-average"))
    val prodArea = NodeSpec("Key agricultural production areas")
    val rainfall3 = NodeSpec("onset of rainfall", Quant("erratic"))
    val drySpell2 = NodeSpec("dry spells in June", Quant("prolonged"))
    val maize = NodeSpec("maize yields", Quant("poor", "particularly"))

    behavior of "TestDoc3 Paragraph 11"

    // tests here
    // now doing chaining
//    failingTest should "have correct edges 1" taggedAs(Vikas) in {
//      tester.test(EdgeSpec(rainfall, Causal, growSeason)) should be (successful) // Test edges connecting them
//    }
    failingTest should "have correct edges 2" taggedAs(Vikas) in {
      tester.test(EdgeSpec(growSeason, Causal, production)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 3" taggedAs(Vikas) in {
      tester.test(EdgeSpec(drySpell, Causal, production)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 4" taggedAs(Vikas) in {
      tester.test(EdgeSpec(rainfall2, Causal, production)) should be (successful) // Test edges connecting them
    }
    affectEventTest should "have correct edges 5" taggedAs(Vikas) in {
      tester.test(EdgeSpec(rainfall3, Causal, prodArea)) should be (successful) // Test edges connecting them
    }
    affectEventTest should "have correct edges 6" taggedAs(Vikas) in {
      tester.test(EdgeSpec(drySpell2, Causal, prodArea)) should be (successful) // Test edges connecting them
    }
    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
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

    val tester = new GraphTester(text)

    // Nodes here
    val rainfall = NodeSpec("rainfall", Quant("average to above average"))
    val rainfall2 = NodeSpec("rainfall", Dec("below-average"), Quant("below-average"))
    val rainfall3 = NodeSpec("rainfall forecast", Inc("above-average"), Quant("above-average")) //Note: Adding rainfall forecast here
    val cropProd = NodeSpec("crop production prospects", Inc("improve"))
    val pastures = NodeSpec("pasture conditions", Inc("improve"))
    val planting = NodeSpec("planting", Quant("delayed", "significantly"))
    val crops = NodeSpec("late-planted crops", Quant("good condition"), Quant("maturity stages of grain filling")) //todo: Check this, do we even need this?
    val rainfall4 = NodeSpec("rainfall", Quant("widespread"))
    val harvest2 = NodeSpec("harvesting") // todo: in the future, prob need Temporal attachment, then (rainfall4, Causal, temporally-modified harvest2)
    val drying2 = NodeSpec("drying activities")

    behavior of "TestDoc3 Paragraph 12"

    // tests here
    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(rainfall)
    }
    passingTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(rainfall2)
    }
    futureWorkTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall, Causal, cropProd)) should be (successful) // Test edges connecting them
    }
    futureWorkTest should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall, Causal, pastures)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 3" taggedAs(Becky) in {
      tester.test(EdgeSpec(rainfall3, Causal, cropProd)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 4" taggedAs(Becky) in {
      tester.test(EdgeSpec(rainfall3, Causal, pastures)) should be (successful) // Test edges connecting them
    }
    passingTest should "have correct singleton node 3" taggedAs(Somebody) in {
      tester.test(planting)
    }
    passingTest should "have correct singleton node 4" taggedAs(Somebody) in {
      tester.test(crops)
    }
    futureWorkTest should "have correct edges 5" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall4, Causal, harvest2))
    }
    futureWorkTest should "have correct edges 6" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall4, Causal, drying2))
    }
  }

  {
    // Paragraph 13
    val text ="""
        |In Sudan, seasonal rainfall has continued to be above average during the past few weeks, which has been
        |favorable in most cropping areas of the country. The weekly forecasts indicate continued intensification
        |of seasonal rainfall, with increased likelihood for flooding in flood-prone areas of eastern Sudan.
        |However, there are localized areas in western Darfur where July rains were below average, resulting in
        |drier-than-normal vegetation conditions. In the coming weeks, the seasonal rains are expected to intensify
        |and may help ease the current dry conditions in parts of western Darfur.
        """

    val tester = new GraphTester(text)

    // Nodes here
    val rainfall = NodeSpec("seasonal rainfall", Inc("above average"), Quant("average"))
    val rainfall2 = NodeSpec("seasonal rainfall", Inc("intensification"))
    val flooding = NodeSpec("likelihood for flooding in flood-prone areas of eastern Sudan", Inc("increased"))
    val rainfall3 = NodeSpec("July rains", Dec("below average"), Quant("average"))
    val vegetation = NodeSpec("drier-than-normal vegetation conditions")
    val rainfall4 = NodeSpec("seasonal rains", Inc("intensify"))
    val dry = NodeSpec("current dry conditions in parts of western Darfur", Quant("ease"))

    behavior of "TestDoc3 Paragraph 13"

    // tests here
    // no longer testing singleton nodes
//    passingTest should "have correct singleton node 1" taggedAs(Ajay) in {
//      tester.test(rainfall) should be (successful) // Test edges connecting them
//    }
    failingTest should "have correct edges 2" taggedAs(Ajay) in {
      tester.test(EdgeSpec(rainfall2, Causal, flooding)) should be (successful) // Test edges connecting them
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

    val tester = new GraphTester(text)

    // Nodes here
    val rainfall = NodeSpec("rainfall season", Inc("above"), Quant("above average"))
    val vegetation = NodeSpec("Vegetation conditions", Dec("below"))

    val vegetation2 = NodeSpec("vegetation conditions", Inc("above-average"), Quant("above-average")) // Increase??

    val rainfallForecasts = NodeSpec("short-and long-term rainfall forecasts", Quant("favorable"), Inc("favorable"), Dec("short-and"))
    val agriculturalAreas = NodeSpec("agricultural areas of the western and central highlands", Inc("favorable"))

    val production = NodeSpec("production of most crops", Quant("likely"), Dec("limited"), Quant("most"))
    val insecurity = NodeSpec("insecurity")
    val availability = NodeSpec("lack of availability and/or access to farm inputs", Dec("lack"))
    //val longFarmInput = NodeSpec("access to farm inputs due to ongoing conflict", Dec("lack"), Quant("ongoing"))

//    val shortFarmInput = NodeSpec("access to farm inputs", Dec("lack"))
    val conflict = NodeSpec("ongoing conflict", Quant("ongoing"))

    behavior of "TestDoc3 Paragraph 14"

    // tests here
//    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
//      tester.test(rainfall)
//    }
    passingTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(vegetation)
    }
    passingTest should "have correct singleton node 3" taggedAs(Somebody) in {
      tester.test(vegetation2)
    }
    passingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfallForecasts, Causal, agriculturalAreas)) should be (successful)
    }
    passingTest should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, production)) should be (successful)
    }
    passingTest should "have correct edges 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(insecurity, Causal, production)) should be (successful)
    }
    passingTest should "have correct edges 4" taggedAs(Somebody) in {
      tester.test(EdgeSpec(availability, Causal, production)) should be (successful)
    }
//    passingTest should "have correct edges 5" taggedAs(Somebody) in {
//      tester.test(EdgeSpec(shortFarmInput, Causal, production)) should be (successful)
//    }
    passingTest should "have correct edges 6" taggedAs(Ajay) in {
      tester.test(EdgeSpec(conflict, Causal, availability)) should be (successful)
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

    val tester = new GraphTester(text)

    // Nodes here
    val rainfall = NodeSpec("seasonal rains", Inc("intensification"))
    val rainfallDeficit = NodeSpec("rainfall", Dec("deficits"), Dec("ease"))
    val rainfall2 = NodeSpec("rains", Quant("heavy"))
    val maize = NodeSpec("maize harvesting", Dec("hamper"))
    val drying = NodeSpec("drying activities", Dec("hamper"))
    val losses = NodeSpec("post-harvest losses")
    val dry = NodeSpec("dry days", Quant("significant")) // todo: this is a part of the hyper edge, should handle one day

    behavior of "TestDoc3 Paragraph 15"

    // tests here
    failingTest should "have correct edges 1" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall, Causal, rainfallDeficit)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 2" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall2, Causal, maize)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 3" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall2, Causal, drying)) should be (successful) // Test edges connecting them
    }
    failingTest should "have correct edges 4" taggedAs(Somebody) in {
      tester.test(EdgeSpec(rainfall2, Causal, losses)) should be (successful) // Test edges connecting them
    }
    passingTest should "have correct singleton node 2" taggedAs(Somebody) in {
      tester.test(dry)
    }
    //todo: "if the rains are sustained without significant dry days in coming weeks." --> future tests with hyper edge and negation


  }

  {
    // Paragraph 16
    val text ="""
        |The coastal strip of Kenya and southern Somalia are also likely to receive light to moderate rains.
        |The rest of eastern Horn is forecast to remain generally sunny and dry, as is seasonally normal.
        """

    val tester = new GraphTester(text)

    // Nodes here
    val rainfall = NodeSpec("rains", Quant("light to moderate"))

    behavior of "TestDoc3 Paragraph 16"

    // tests here
    passingTest should "have correct singleton node 1" taggedAs(Somebody) in {
      tester.test(rainfall)
    }

  }
}
