package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.test.TestUtils.{ieSystem, _}

import scala.util.Random

class TestSerial extends Test {

  // This text has been found to be problematic
  protected val texts = Array(
    "South Sudan's principal trading partner is Uganda (Table 2.5). Customs officials believe that 80 percent of all goods imported into South Sudan enter through the Nimule-Torit-Juba route (WFP 2015). Trade with Sudan, South Sudan's second most important trading partner, has decreased significantly since independence. Trade between the two countries now is mainly informal. Most food from Sudan is transported from El Obeid to Aweil. Before the separation of the two countries, northern Sudan was a key provider of both food and fuel for southern Sudan. However, in 2012 the border was closed and supply was cut off (World Bank 2014). Since 2013 some borders have been reopened, but trade has been slow to recover. Kenya's trade with South Sudan is mainly in nonfood items. Ethiopia also trades with South Sudan, and in opposition-controlled areas, trade is now almost entirely with Ethiopia.",
    "Although about 70 percent of total land area is suitable for cultivation in South Sudan, only 4 percent of the land is currently used for agriculture (Diao et al. 2011, Figure 2.1). Thus, there is ample scope for expansion of area cultivated. Currently, most crop cultivation is done on small farms, producing little if any marketable surplus. According to estimates for 2013 from the Food and Agriculture Organization of the United Nations (FAO) and the World Food Programme (WFP), 10 percent of the population of South Sudan (about 1 million people) are severely food insecure (FAO and WFP 2013).",
    "Rainfall within the country ranges from 500 mm to 2,000 mm per year, with the highest levels of rainfall in the southern part of the country. Much of north central and northeastern South Sudan is located in flood plains along the Nile River (the Sudd) that are flooded during the rainy season but receive little rainfall otherwise (600 to 800 mm per year). The highest potential areas for agricultural production are Western Equatoria and the southern half of Central Equatoria, or the so-called Green Belt, where annual rainfall ranges from 900 to 2,000 mm per year (Table 2.6). Rainfall in the Hills and Mountains region of the northern half of Central Equatoria and the western half of Eastern Equatoria (500 to 800 mm per year) is also sufficient to support substantial crop agriculture (WFP 2011).",
    "In most areas of South Sudan, soils are low in phosphorous as well as in organic matter. Despite this, most soils are moderately fertile. But soils could quickly become nutrient deficient due to leaching related to heavy rainfall, low levels of fertilizer use (on average only 4 kg per ha of fertilizers are used in South Sudan), and over-farming. Nutrient mining from maize production ranges between 30 and 60 kg per ha per year. Smallholders have limited knowledge and capacity, lack an understanding of the role of fertilizer in improved crop production, and do not have access to extension or transfer services--all factors that could lead to further soil nutrient depletion (Kowr 2013).",
    "Institutional weakness has further hindered development of the agriculture sector. Disagreements over land rights for crop cultivation and livestock grazing continue to be a major source of conflict. This insecurity discourages farmers from expanding production, and traders and retailers from building marketing infrastructure. The extremely low level of public investment in recent decades has meant that essentially no irrigation infrastructure exists and only 2 percent of South Sudan's limited road network is paved. Roads are poorly maintained, not repaired, and completely washed out during the rainy season (World Bank 2012; USAID and Fintrac 2012). Because of this inadequate transportation infrastructure, it is difficult and expensive for subsistence farmers to transport surpluses to markets.",
    "Further, poor business practices and a lack of information about market prices make it difficult for businesses to develop along the agriculture value chain. South Sudan receives a ranking of 186 out of 189 on ease of doing business in the World Bank 2015 Doing Business report (World Bank 2014). Furthermore, South Sudan receives a percentile rank of 5.7 in government effectiveness and of 3.0 in control of corruption in the World Bank's governance indicators (World Bank 2015)."
  )

  protected val altTexts = Array(
    "According to the latest IPC analysis, famine conditions, previously reported in February 2017 in former Leer and Mayendit counties in former Unity State, were no longer occurring by late June 2017.Overall, the number of people facing IPC Phase 5: \"Catastrophe\" food security conditions declined from over 100 000 in February to about 45 000 in June due to sustained multi-sectoral humanitarian assistance operations and the two counties are currently classified as IPC Phase 4: \"Emergency\".",
    "However, nationwide, the food insecure caseload (IPC Phases 3, 4 and 5) increased from about 5 million in February to a record high of 6 million in June as food access continues to be severely constrained by widespread insecurity, large scale displacements, high food prices, market disruptions, macro-economic collapse and exhaustion of households' coping mechanisms. The areas of major concern are Greater Jonglei and Unity states, where over 60 percent of the population faces \"Crisis\", \"Emergency\" and \"Catastrophe\" levels of food insecurity. In particular, the people facing catastrophic conditions are located in Ayod County in Greater Jonglei State and in Leer, Koch and Mayendit counties in Unity State.",
    "Since the start of the conflict in mid-December 2013, about 3.9 million people were forced to flee their homes due to insecurity, including about 1.9 million IDPs and 2 million that sought refuge in neighbouring countries (Uganda, the Sudan, the Democratic Republic of the Congo, Ethiopia and Kenya).",
    "In southern bi-modal rainfall areas, harvesting of first season crops was concluded in August. Seasonal rains were above-average in the \"green belt\", including the former Central and Western Equatoria states, while in the former Eastern Equatoria State they started in late April with about a one-month delay. In northern and central uni-modal rainfall areas, harvesting of short cycle sorghum and maize crops has recently started, while long cycle sorghum crops will be gathered from November to January. Weather conditions have been generally favourable so far as seasonal rains have been average to above average, thus benefiting vegetation conditions.",
    "However, prospects for 2017 aggregate cereal production are generally unfavourable as agricultural activities continue to be severely affected by the protracted and widespread insecurity, which is constraining farmers' access to fields and is causing large scale displacement of people, input shortages and damage to households' productive assets. In the traditionally surplus-producing areas of southern Greater Equatoria Region, crop production is expected to be lower than the already poor 2016 output due to recent massive displacements outside the former Central and Eastern Equatoria states. Notably, about 75 percent of the population of the former Central Equatoria State has reportedly left their living areas.",
    "In addition, Fall Armyworm infestations have been reported in all regions of the country, with significant crop damage, especially in parts of former Northern Bahr el Ghazal, Eastern Equatoria and Central Equatoria states.",
    "In the capital, Juba, prices of maize and sorghum more than doubled in the first semester of 2017, reaching record levels in June, driven by a tight supply situation, market disruptions, hyperinflation and a significant depreciation of the local currency. Subsequently, they declined by about 12 percent between June and August, following the first season harvest in southern bi-modal rainfall areas and the establishment, by the Government, of a trading company selling basic food commodities at subsidized prices. Prices of groundnuts decreased by 22 percent over the same period, while prices of wheat flour continued to soar in recent months, reaching new record highs in August. Overall, prices of these food staples in August were more than twice the high levels in August last year and up to 12 times higher than in the corresponding period two years earlier."
  )

  protected def toText(eidosSystem: EidosSystem): String = {
    val annotatedDocuments = texts.map(eidosSystem.extractFromText(_))
    val corpus = new JLDCorpus(annotatedDocuments, ieSystem)
    val result = corpus.toJsonStr

    Random.shuffle(altTexts.toList).map(eidosSystem.extractFromText(_))
    result
  }

  {
    def innerToText = toText(ieSystem)
//    def innerToText = toText(new EidosSystem())

    val expected = innerToText

    behavior of "serial EidosSystem calling of annotate"

    it should "be consistent" in {
      for (_ <- 1 to 5) {
        val actual = innerToText

        expected should be(actual)
      }
    }
  }
}
