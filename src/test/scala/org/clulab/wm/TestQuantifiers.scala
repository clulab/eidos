package org.clulab.wm

import org.scalatest._
import TestUtils._
import ReaderUtils._


/**
  * Tests for Quantifiers (our gradable adjectives) in RAPS texts.
  */
class TestQuantifiers extends FlatSpec with Matchers {

  //ONE Quantifier
  val quant2 = "Soil quality will decline by a small-to-medium extent"
  val quant7 = "The government promotes high-yielding and drought-/flood-tolerant rice varieties with policy to encourage the application of organic fertilizers, decreasing the cost on inorganic fertilizers."
  val quant10 = "Imports are inadequate to meet domestic demand."
  val quant11 = "A combination of increasing population, government plans to invest in fertilizer factory, government subsidy on fertilizers, improved economic performance expected to cause a shift from agriculture to service industry, government plans for massive expansion of irrigation (irrigate 1 million ha.),"
  val quant12 = "However, opportunities for massive increases in agricultural production and productivity exist."
  val quant18 = "Farmers produce beyond subsistence, but fail to access profitable markets (inputs and outputs)"
  val quant20 = "Main interventions will include support for the agricultural-service sector, fertilizer subsidies, and feeder roads (slow)."
  val quant21 = "Investment in infrastructure is, however, slow."
  val quant29 = "Hence, government liberalizes imports of food grains, invests in food chain logistics, and boost research and development for new crop cultivars to boost agricultural production for ensuring food security."
  val quant32 = "Climate change remains as a key challenge for a country like Nepal where subsistence-based and rainfed agriculture system is dominant"
  val quant35 = "Significant decline in poverty will be associated with a decrease in family size and increase in non-farm income."
  val quant37 = "The transformative path will lead to emerging agricultural powerhouse in West Africa with reliance on strong agro-dealers and satisfactory solutions to consumer preferences."
  val quant38 = "Heavy reliance on suitable climatic conditions for agricultural production always imposes serious risk to the agricultural sector in Nepal."

  "TestQuant_1" should "extract 1 Quantifier from text" in {
    val mentions2 = extractMentions(quant2)
    val entities2 = mentions2.filter(_ matches QUANT) //small-to-medium
    entities2 should have size(1)

    val mentions7 = extractMentions(quant7)
    val entities7 = mentions7.filter(_ matches QUANT) //high-yielding
    entities7 should have size(1)

    val mentions10 = extractMentions(quant10)
    val entities10 = mentions10.filter(_ matches QUANT) //inadequate
    entities10 should have size(1)

    val mentions11 = extractMentions(quant11)
    val entities11 = mentions11.filter(_ matches QUANT) //massive
    entities11 should have size(1)

    val mentions12 = extractMentions(quant12)
    val entities12 = mentions12.filter(_ matches QUANT) //massive
    entities12 should have size(1)

    val mentions18 = extractMentions(quant18)
    val entities18 = mentions18.filter(_ matches QUANT)
    entities18 should have size(1)

    val mentions20 = extractMentions(quant20)
    val entities20 = mentions20.filter(_ matches QUANT)
    entities20 should have size(1)

    val mentions21 = extractMentions(quant21)
    val entities21 = mentions21.filter(_ matches QUANT)
    entities21 should have size(1)

    val mentions29 = extractMentions(quant29)
    val entities29 = mentions29.filter(_ matches QUANT)
    entities29 should have size(1)


    val mentions32 = extractMentions(quant32)
    val entities32 = mentions32.filter(_ matches QUANT)
    entities32 should have size(1)

    val mentions35 = extractMentions(quant35)
    val entities35 = mentions35.filter(_ matches QUANT)
    entities35 should have size(1)

    val mentions37 = extractMentions(quant37)
    val entities37 = mentions37.filter(_ matches QUANT)
    entities37 should have size(1)

    val mentions38 = extractMentions(quant38)
    val entities38 = mentions38.filter(_ matches QUANT)
    entities38 should have size(1)

  }



  // TWO Quantifiers
  val quant1 = "There will be a small increase in crop diversity due to the need to combat the climate and market risks as both of these might become more volatile in the future."
  val quant3 = "Other programs are the Beira Agricultural Growth Corridor for small to medium companies."
  val quant4 = "Unintended government policy consequences; lack of good farm management practices specifically to biophysical conditions of land lead to small benefit to the livelihoods."
  val quant5 = "Agricultural inputs are not affordable for small-scale farmers"
  val quant6 = "A proactive legislation will stipulate land-tenure security, incentives for the banking sector, and revamp research and extension to promote productivity-enhancing technologies for adoption on a large scale."
  val quant8 = "Better and well-functioning agricultural credit and market services for both established and emerging farmers."
  val quant13 = "Poor infrastructure is a major barrier to agricultural development."
  val quant15 = "Most subsidies are likely to decline while prices of agricultural commodities will increase."
  val quant16 = "Agricultural inputs are in short supply and expensive."
  val quant22 = "Poverty levels continue to increase, people become more vulnerable to food insecurity and other risks."
  val quant23 = "Use of improved cultivars and mechanization will be increased and use of critical interventions may lead to increases in productivity and efficient use of resources."
  val quant24 = "This RAP assumes dominance of state actors in the agricultural-development agenda with the view to bring in fast short-term gains with food-security outcomes to the population."
  val quant26 = "Lack of competition input prices tend to be high, output prices generally low."
  val quant30 = "Poor road construction and maintenance restrict private-sector investments in these high-potential agricultural areas (for crops and livestock)."
  val quant34 = "There will not be significant changes in food imports, while yield of important crops will increase due to technological progress in agriculture. "
  val quant36 = "Most people remain in rural areas where agriculture is the main livelihood activity due to lack of alternatives."

  "TestQuant_2" should "extract 2 Quantifiers in texts" in  {

    val mentions1 = extractMentions(quant1)
    val entities1 = mentions1.filter(_ matches QUANT) //2: small, more, (not sure if we should get "volatile"?)
    entities1 should have size(2)

    val mentions3 = extractMentions(quant3)
    val entities3 = mentions3.filter(_ matches QUANT) //2: small & medium
    entities3 should have size(2)

    val mentions4 = extractMentions(quant4)
    val entities4 = mentions4.filter(_ matches QUANT)   //2: "small" and "good"
    entities4 should have size(2)

    val mentions5 = extractMentions(quant5)
    val entities5 = mentions5.filter(_ matches QUANT)  //2: affordable and small-scale
    entities5 should have size(2)

    val mentions6 = extractMentions(quant6)
    val entities6 = mentions6.filter(_ matches QUANT)   //productivity-enhancing , large
    entities6 should have size(2)

    val mentions8 = extractMentions(quant8)
    val entities8 = mentions8.filter(_ matches QUANT)   //2: better and well-functioning
    entities8 should have size(2)

    val mentions13 = extractMentions(quant13)
    val entities13 = mentions13.filter(_ matches QUANT) //2: poor and major
    entities13 should have size(2)


    val mentions15 = extractMentions(quant15)
    val entities15 = mentions15.filter(_ matches QUANT) //Most and likely
    entities15 should have size(2)

    val mentions16 = extractMentions(quant16)
    val entities16 = mentions16.filter(_ matches QUANT) //short, expensive
    entities16 should have size(2)

    val mentions22 = extractMentions(quant22)
    val entities22 = mentions22.filter(_ matches QUANT)   //more, vulnerable
    entities22 should have size(2)

    val mentions23 = extractMentions(quant23)
    val entities23 = mentions23.filter(_ matches QUANT) // efficient, critical
    entities23 should have size(2)

    val mentions24 = extractMentions(quant24)
    val entities24 = mentions24.filter(_ matches QUANT) //short-term and fast
    entities24 should have size(2)


    val mentions26 = extractMentions(quant26)
    val entities26 = mentions26.filter(_ matches QUANT) // high, low
    entities26 should have size(2)

    val mentions30 = extractMentions(quant30)
    val entities30 = mentions30.filter(_ matches QUANT) //poor, high-potential
    entities30 should have size(2)

    val mentions34 = extractMentions(quant34)
    val entities34 = mentions34.filter(_ matches QUANT) //significant & important
    entities34 should have size(2)

    val mentions36 = extractMentions(quant36)
    val entities36 = mentions36.filter(_ matches QUANT) //most, rural
    entities36 should have size(2)


  }


  //THREE Quantifiers
  val quant9 = "The governmental policy objective is to achieve food security, ensure adequate raw materials for the manufacturing sector, and increased export earnings through increased productivity, efficient input use, and better market access, infrastructure, and service development."
  val quant17 = "Though the government adopts long-term and short-term policy measures, rice–wheat production costs increase substantially."
  val quant19 = "With a high cost of production and degraded natural resources, profitability in agriculture may be further reduced, making agriculture unprofitable."
  val quant27 = "Limited financial capacities and low education levels further restrict farmers’ ability for higher benefits from increased agricultural production."
  val quant28 = "The government promotes improved cultivar and climate-smart technologies but the policy to cut down the use of inorganic fertilizer and phase out the fertilizer subsidy results in deteriorating biophysical conditions, low use of inorganic fertilizer, less water, reduced farm sizes which lead to low benefit from the improved cultivar."
  val quant31 = "On the other hand, having limited capacity to adapt and respond to the climatic stresses, rural poor farmers in the country face the challenge of adapting to climate change impacts"
  val quant33 = "Agriculture production is very important to ensure food security and provision of employment opportunities to the majority of the rural population."

  "TestQuant_3" should "extract 3 Quantifiers in texts" in  {
    val mentions9 = extractMentions(quant9)
    val entities9 = mentions9.filter(_ matches QUANT)   //3:  adequate, efficient, better
    entities9 should have size(3)

    val mentions17 = extractMentions(quant17)
    val entities17 = mentions17.filter(_ matches QUANT)  //2: long-term and short-term, substantially
    entities17 should have size(3)

    val mentions19 = extractMentions(quant19)
    val entities19 = mentions19.filter(_ matches QUANT)   //3: high, degraded, unprofitable
    entities19 should have size(3)

    val mentions27 = extractMentions(quant27)
    val entities27 = mentions27.filter(_ matches QUANT)  //limited, low, higher
    entities27 should have size(3)

    val mentions28 = extractMentions(quant28)
    val entities28 = mentions28.filter(_ matches QUANT)  //low x2, less
    entities28 should have size(3)

    //Fails because "limited" is VBN
//    val mentions31 = extractMentions(quant31)
//    val entities31 = mentions31.filter(_ matches QUANT)  //limited, rural, poor
//    entities31 should have size(3)

    val mentions33 = extractMentions(quant33)
    val entities33 = mentions33.filter(_ matches QUANT) //important, rural, majority
    entities33 should have size(3)


  }



  //SIX Quantifiers
  val quant14 =  "The transformation however starts under extremely difficult conditions, characterized by large account deficit and liquidity challenges and limited direct foreign investment due to lack of clarity on investment security and high interest rates."

  "TestQuant_6" should "extract 6 Quantifiers in texts" in  {

    val mentions14 = extractMentions(quant14)
    val entities14 = mentions14.filter(_ matches QUANT) //extremely, difficult, large, limited, high, direct,
    entities14 should have size(6)

  }



}
