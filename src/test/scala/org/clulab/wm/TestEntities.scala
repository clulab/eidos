package org.clulab.wm


import org.scalatest._
import TestUtils._
import ReaderUtils._


class TestEntities extends FlatSpec with Matchers {


  //Farm-size
  val sent1a = "Farm size and wage rates will increase."
  val sent1b = "The government promotes improved cultivar and climate-smart technologies but the policy to cut down the use of inorganic fertilizer and phase out the fertilizer subsidy results in deteriorating biophysical conditions, low use of inorganic fertilizer, less water, reduced farm sizes which lead to low benefit from the improved cultivar."
  val sent1c = "Soil quality will decline by a small-to-medium extent, due to pollution, and intensive cultivation will be caused by a shrinking land base for agriculture."


  "sent1a" should "extract Farm-size entity mention" in {

    val mentions1a = extractMentions(sent1a)
    val entities1a = mentions1a.filter(_ matches FARM_SIZE) //#Vector[Mention] is Vector[TextBoundMentions]

    entities1a should have size (1)
    val text1a = entities1a.head.text
    text1a should be("Farm size")
  }

  "sent1b" should "extract Farm-size entity mention" in {
    val mentions1b = extractMentions(sent1b)
    val entities1b = mentions1b.filter(_ matches FARM_SIZE) //farm sizes

    entities1b should have size(1)

    val text = entities1b.head.text
    text should be("farm sizes")

  }

  "sent1c" should "extract Farm-size entity mention" in {
    val mentions1c = extractMentions(sent1c)
    val entities1c = mentions1c.filter(_ matches FARM_SIZE) //farm sizes

    entities1c should have size(1)

    val text = entities1c.head.text
    text should be("land base for agriculture")

  }


  //Fertilizer-Use
  val sent2a = "Fertilizer-use intensity and fertilizer productivity will increase."
  val sent2b = "The government promotes high-yielding and drought-/flood-tolerant rice varieties with policy to encourage the application of organic fertilizers, decreasing the cost on inorganic fertilizers."
  val sent2c = "The government promotes improved cultivar and climate-smart technologies but the policy to cut down the use of inorganic fertilizer and phase out the fertilizer subsidy results in deteriorating biophysical conditions, low use of inorganic fertilizer, less water, reduced farm sizes which lead to low benefit from the improved cultivar."

  "sent2a" should "extract FertilizerUse entity mention" in {
    val mentions2a = extractMentions(sent2a)
    val entities2a = mentions2a.filter(_ matches FERT_USE) //fertilizer-use
    entities2a should have size (1)
    val text = entities2a.head.text
    text should be("Fertilizer-use intensity")
  }

  "sent2b" should "extract FertilizerUse entity mention" in {
    val mentions2b = extractMentions(sent2b)
    val entities2b = mentions2b.filter(_ matches FERT_USE) //application of organic fertilizers
    entities2b should have size (1)
    val text = entities2b.head.text
    text should be("the application of organic fertilizers")

  }

  "sent2c" should "extract 2 FertilizerUse entity mentions" in {

    val mentions2c = extractMentions(sent2c)
    val entities2c = mentions2c.filter(_ matches FERT_USE) //use of inorganic fertilizer
    entities2c should have size (2) //Vector[Mention]
    val text = entities2c.map(_.text) //Vector[String]
    text should contain("the use of inorganic fertilizer")
    text should contain("use of inorganic fertilizer")

  }



  //Fertlizer-Price
  val sent3 = "The government promotes high-yielding and drought-/flood-tolerant rice varieties with policy to encourage the application of organic fertilizers, decreasing the cost on inorganic fertilizers."

  "send3" should "extract FertilizerPrice entity mention" in {

    val mentions3 = extractMentions(sent3)
    val entities3 = mentions3.filter(_ matches FERT_PRICE) //cost on inorganic fertilizers
    entities3 should have size (1)
    val text = entities3.head.text
    text should be("the cost on inorganic fertilizers")
  }


  //HerdSize
  val sent4 = "The buffalo herd size will increase" //sentence not actually in RAPS texts

  "sent4" should "extract HerdSize entity mention" in {
    val mentions4 = extractMentions(sent4)
    val entities4 = mentions4.filter(_ matches HERD_SIZE)
    entities4 should have size (1)
    val text = entities4.head.text
    text should be("The buffalo herd size")

  }

  //HouseholdSize
  val sent5 = "The government aims to invest more in agriculture, shortage of labor with the consequence of decreased population growth and household size."
  val sent5b = "With increases in poverty levels people become more vulnerable to climate change and other risks"

  "sent5" should "extract 2 HouseholdSize entity mentions" in {
    val mentions5 = extractMentions(sent5)
    val entities5 = mentions5.filter(_ matches HOUSE_SIZE) //household size & population
    entities5 should have size (2)
    val text = entities5.map(_.text)
    text should contain("population growth")
    text should contain("household size")
  }

  "sent5b" should "extract 1 HouseholdSize entity mention" in {
    val mentions5 = extractMentions(sent5b)
    val entities5 = mentions5.filter(_ matches HOUSE_SIZE)
    entities5 should have size (1)
    val text = entities5.map(_.text)
    text should contain("people")
  }


  //Labor
  val sent6a = "Labor migration and HIV/AIDS result in labor shortage."
  val sent6b = "The government aims to invest more in agriculture, shortage of labor with the consequence of decreased population growth and household size." //labor

  "sent6a" should "extract 2 Labor entity mentions" in {
    val mentions6a = extractMentions(sent6a)
    val entities6a = mentions6a.filter(_ matches LABOR) //Labor migration & labor shortage
    entities6a should have size (2)
    val text = entities6a.map(_.text)
    text should contain("Labor migration")
    text should contain("labor")
  }

  "sent6b" should "extract Labor entity mention" in {

    val mentions6b = extractMentions(sent6b)
    val entities6b = mentions6b.filter(_ matches LABOR)
    entities6b should have size (1)
    val text = entities6b.head.text
    text should be("labor")

  }


  //PestDisease
  val sent7 = "The disease killed hundreds" // not in RAPS text.

  "sent7" should "extract PestDisease entity mention" in {
    val mentions7 = extractMentions(sent7)
    val entities7 = mentions7.filter(_ matches PEST_DISEASE) //disease
    entities7 should have size (1)
    val text = entities7.head.text
    text should be("The disease")
  }

  //Productivity
  val sent8a = "Government policy objective is to achieve food security, ensure adequate raw materials for the manufacturing sector and increased export earnings through increased productivity, efficient input use, improved investment and market access, infrastructure, and service development, targeting annual agricultural growth of 9.1% by 2015." //Productivity
  val sent8b = "A proactive legislation will stipulate land-tenure security, incentives for the banking sector, and revamp research and extension to promote productivity-enhancing technologies for adoption on a large scale." //Productivity
  val sent8c = "The adoption process will be instigated due to the anticipated losses in agricultural productivity in the face of climatic uncertainties."
  val sent8d = "Fertilizer-use intensity and fertilizer productivity will increase."
  val sent8e = "However, opportunities for massive increases in agricultural production and productivity exist."
  val sent8f = "Use of improved cultivars and mechanization will be increased and use of critical interventions may lead to increases in productivity and efficient use of resources."
  val sent8g = "Opportunities for massive increases in agricultural production and productivity exist but are not being exploited."

  "sent8a" should "extract Productivity entity mention" in {
    val mentions8a = extractMentions(sent8a)
    val entities8a = mentions8a.filter(_ matches PRODUCTIVITY) //increase productivity
    entities8a should have size (1) //
    val text = entities8a.head.text
    text should be("productivity")
  }


  "sent8b" should "extract Productivity entity mention" in {
    val mentions8b = extractMentions(sent8b)
    val entities8b = mentions8b.filter(_ matches PRODUCTIVITY) //productivity-enhancing technologies
    entities8b should have size (1)
    val text = entities8b.head.text
    text should be("productivity-enhancing technologies")

  }

  "sent8c" should "extract Productivity entity mention" in {

    val mentions8c = extractMentions(sent8c)
    val entities8c = mentions8c.filter(_ matches PRODUCTIVITY) //agricultural productivity
    entities8c should have size (1)
    val text = entities8c.head.text
    text should be("agricultural productivity")
  }

  "sent8d" should "extract Productivity entity mention" in {
    val mentions8d = extractMentions(sent8d)
    val entities8d = mentions8d.filter(_ matches PRODUCTIVITY) //fertilizer productivity
    entities8d should have size (1)
    val text = entities8d.head.text
    text should be("fertilizer productivity")
  }

  "sent8e" should "extract 2 Productivity entity mentions" in {
    val mentions8e = extractMentions(sent8e)
    val entities8e = mentions8e.filter(_ matches PRODUCTIVITY) //production & productivity
    entities8e should have size (2)
    val text = entities8e.map(_.text)
    text should contain("agricultural production")
    text should contain("productivity")

  }

  "sent8f" should "extract 2 Productivity mentions" in {
    val mentions8f = extractMentions(sent8f)
    val entities8f = mentions8f.filter(_ matches PRODUCTIVITY) //productivity
    entities8f should have size (2) //productivity AND mechanization (indirect driver of productivity)
    val text = entities8f.map(_.text)
    text should contain("mechanization")
    text should contain("productivity")
  }

  "sent8g" should "extract 2 Productivity mentions" in {
    val mentions8g = extractMentions(sent8g)
    val entities8g = mentions8g.filter(_ matches PRODUCTIVITY) //fertilizer productivity
    entities8g should have size (2) //production and productivity
    val text = entities8g.map(_.text)
    text should contain("agricultural production")
    text should contain("productivity")

  }



  //Soil
  val sent9 = "Soil quality will decline by a small-to-medium extent, due to pollution, and intensive cultivation will be caused by a shrinking land base for agriculture."

  "sent9" should "extract Soil entity mention" in  {
    val mentions9 = extractMentions(sent9)
    val entities9 = mentions9.filter(_ matches SOIL) //soil quality
    entities9 should have size (1)
    val text = entities9.head.text
    text should be("Soil quality")

  }

  //Subsidy
  val sent10a = "In agriculture, government promotes market-oriented production; subsidies are only during recovery and rehabilitation."
  val sent10b = "A combination of increasing population, government plans to invest in fertilizer factory, government subsidy on fertilizers, improved economic performance expected to cause a shift from agriculture to service industry, government plans for massive expansion of irrigation (irrigate 1 million ha.), newly devolved county governments etc. are some of the developments expected to change agriculture development in the country."
  val sent10c = "Main interventions will include support for the agricultural-service sector, fertilizer subsidies, and feeder roads (slow)."
  val sent10d = "The government promotes improved cultivar and climate-smart technologies but the policy to cut down the use of inorganic fertilizer and phase out the fertilizer subsidy results in deteriorating biophysical conditions, low use of inorganic fertilizer, less water, reduced farm sizes which lead to low benefit from the improved cultivar."
  val sent10e = "Along with the support programs such as agricultural insurance and input subsidies, the government efforts and investments will be increased for extending irrigation services, agricultural mechanization, and developing disaster risk-management practices."
  val sent10f = "Most subsidies are likely to decline while prices of agricultural commodities will increase."

  "mentions10a" should "extract Subsidy entity mention" in {
    val mentions10a = extractMentions(sent10a)
    val entities10a = mentions10a.filter(_ matches SUBSIDY) //subsidies
    entities10a should have size (1)
    val text = entities10a.head.text
    text should be("subsidies")
  }

  "mentions10b" should "extract Subsidy entity mention" in {
    val mentions10b = extractMentions(sent10b)
    val entities10b = mentions10b.filter(_ matches SUBSIDY) //subsidy
    entities10b should have size (1)
    val text = entities10b.head.text
    text should be("government subsidy on fertilizers")
  }

  "mentions10c" should "extract Subsidy entity mention" in {
    val mentions10c = extractMentions(sent10c)
    val entities10c = mentions10c.filter(_ matches SUBSIDY) // subsidies
    entities10c should have size (1)
    val text = entities10c.head.text
    text should be("fertilizer subsidies")
  }

  "mentions10d" should "extract Subsidy entity mention" in {
    val mentions10d = extractMentions(sent10d)
    val entities10d = mentions10d.filter(_ matches SUBSIDY) //fertilizer subsidy
    entities10d should have size (1)
    val text = entities10d.head.text
    text should be("the fertilizer subsidy")
  }

  "mentions10e" should "extract 2 Subsidy entity mentions" in {
    val mentions10e = extractMentions(sent10e)
    val entities10e = mentions10e.filter(_ matches SUBSIDY) //subsidies & investments
    entities10e should have size (2)
    val text = entities10e.map(_.text)
    text should contain("input subsidies")
    text should contain("investments")
  }

  "mentions10f" should "extract Subsidy entity mention" in {
    val mentions10f = extractMentions(sent10f)
    val entities10f = mentions10f.filter(_ matches SUBSIDY) //most subsidies
    entities10f should have size (1)
    val text = entities10f.head.text
    text should be("subsidies")
  }



  //Water
  val sent11a = "Government puts more emphasis on improving the agricultural water irrigation/management system to cope with drought conditions."
  val sent11b = "The government promotes improved cultivar and climate-smart technologies but the policy to cut down the use of inorganic fertilizer and phase out the fertilizer subsidy results in deteriorating biophysical conditions, low use of inorganic fertilizer, less water, reduced farm sizes which lead to low benefit from the improved cultivar."
  //5 mentions of "water" in sent11c!
  val sent11c = "Water quality and water availability for agriculture will decrease due to pollution of water bodies, and competition for water from other sources, but water-use efficiency in agriculture will increase due to technological progress."

  "sent11a" should "extract Water entity mention" in {
    val mentions11a = extractMentions(sent11a)
    val entities11a = mentions11a.filter(_ matches WATER) //water irrigation/management system
    entities11a should have size (1)
    val text = entities11a.head.text
    text should be("the agricultural water irrigation/management system")
  }

  "sent11b" should "extract Water entity mention" in {
    val mentions11b = extractMentions(sent11b)
    val entities11b = mentions11b.filter(_ matches WATER) //less water
    entities11b should have size (1)
    val text = entities11b.head.text
    text should be("water")
  }

  "sent11c" should "extract 4 Water entity mentions" in {
    val mentions11c = extractMentions(sent11c)
    val entities11c = mentions11c.filter(_ matches WATER) //
    val text = entities11c.map(_.text)
    text should contain("Water quality")
    text should contain("water availability for agriculture")
    text should contain("competition for water from other sources")
    text should contain("water-use efficiency in agriculture")
    entities11c should have size (4)

  }


//  //Government
//  val sent12a = "The government aims to invest more in agriculture, shortage of labor with the consequence of decreased population growth and household size."
//  val sent12b = "The government promotes improved cultivar and climate-smart technologies but the policy to cut down the use of inorganic fertilizer and phase out the fertilizer subsidy results in deteriorating biophysical conditions, low use of inorganic fertilizer, less water, reduced farm sizes which lead to low benefit from the improved cultivar."
//  val sent12c = "Though the government adopts long-term and short-term policy measures, rice–wheat production costs increase substantially. Imports are inadequate to meet domestic demand."
//  val sent12d = "Hence, government liberalizes imports of food grains, invests in food chain logistics, and boost research and development for new crop cultivars to boost agricultural production for ensuring food security."
//  val sent12e = "However, the government will prioritize its programs to minimize the loss from climate change impacts and reduce the vulnerability of the people."
//  val sent12f = "Along with the support programs such as agricultural insurance and input subsidies, the government efforts and investments will be increased for extending irrigation services, agricultural mechanization, and developing disaster risk-management practices."
//  val sent12g = "Persisting economic crisis, governments extractive policies (high taxes), and lack of incentives and security for private-sector investment hinder development."
//
//  "TestEntities_Govmt" should "extract entity mentions (Government) in text" in {
//    val mentions12a = extractMentions(sent12a)
//    val entities12a = mentions12a.filter(_ matches GOVT) //The government
//
//    entities12a should have size (1)
//
//    val mentions12b = extractMentions(sent12b)
//    val entities12b = mentions12b.filter(_ matches GOVT) //the government
//
//    entities12b should have size (1)
//
//    val mentions12c = extractMentions(sent12c)
//    val entities12c = mentions12c.filter(_ matches GOVT) //the government
//
//    entities12c should have size (1)
//
//    val mentions12d = extractMentions(sent12d)
//    val entities12d = mentions12d.filter(_ matches GOVT) //government
//
//    entities12d should have size (1)
//
//    val mentions12e = extractMentions(sent12e)
//    val entities12e = mentions12e.filter(_ matches GOVT) //the government
//
//    entities12e should have size (1)
//
//    val mentions12f = extractMentions(sent12f)
//    val entities12f = mentions12f.filter(_ matches GOVT) //the government
//
//    entities12f should have size (1)
//
//    val mentions12g = extractMentions(sent12g)
//    val entities12g = mentions12g.filter(_ matches GOVT) //governments
//
//    entities12g should have size (1)
//
//  }


  //Climate
  val sent13a = "With increases in poverty levels people become more vulnerable to climate change and other risks."
  val sent13b = "The government aims to improve food security through self-sufficiency in rice with a framework to promote the rice sector to cope with impacts of variable climate."
  val sent13c = "The government promotes improved cultivar and climate-smart technologies but the policy to cut down the use of inorganic fertilizer and phase out the fertilizer subsidy results in deteriorating biophysical conditions, low use of inorganic fertilizer, less water, reduced farm sizes which lead to low benefit from the improved cultivar."
  val sent13d = "Climate change has an adverse impact on agricultural production system in the Indo-Gangetic region where rice–wheat is the predominant cropping system, which contributes to national food security."
  val sent13e = "Global trends suggest that rice–wheat production in the region will be adversely affected by climate change."
  val sent13f = "Climate change remains as a key challenge for a country like Nepal where subsistence-based and rainfed agriculture system is dominant. "
  val sent13g = "On the other hand, having limited capacity to adapt and respond to the climatic stresses, rural poor farmers in the country face the challenge of adapting to climate change impacts."
  val sent13h = "However, the government will prioritize its programs to minimize the loss from climate change impacts and reduce the vulnerability of the people."
  val sent13i = "The support for agricultural research, education, and extension programs will also be increased for developing and disseminating climate change adaptation agricultural technologies to the farmers."
  val sent13j = "This will support them as they adapt to climate change and reduce their vulnerability."
  val sent13k = "There will be a small increase in crop diversity due to the need to combat the climate and market risks as both of these might become more volatile in the future."

  "sent13a" should "extract Climate entity mention" in {
    val mentions13a = extractMentions(sent13a)
    val entities13a = mentions13a.filter(_ matches CLIMATE) //climate change
    entities13a should have size (1)
    val text = entities13a.head.text
    text should be("climate change")
  }

  "sent13b" should "extract Climate entity mention" in {
    val mentions13b = extractMentions(sent13b)
    val entities13b = mentions13b.filter(_ matches CLIMATE) //variable climate
    entities13b should have size (1)
    val text = entities13b.head.text
    text should be("impacts of variable climate")
  }

  //TODO: unsure if it should extract this or not?
  "sent13c" should "extract Climate entity mention" in {
    val mentions13c = extractMentions(sent13c)
    val entities13c = mentions13c.filter(_ matches CLIMATE) //
    entities13c should have size (1)
    val text = entities13c.head.text
    text should be("climate-smart")
  }

  "sent13d" should "extract Climate entity mention" in {
    val mentions13d = extractMentions(sent13d)
    val entities13d = mentions13d.filter(_ matches CLIMATE) //Climate change
    entities13d should have size (1)
    val text = entities13d.head.text
    text should be("Climate change")
  }

  "sent13e" should "extract Climate entity mention" in {
    val mentions13e = extractMentions(sent13e)
    val entities13e = mentions13e.filter(_ matches CLIMATE) //climate change
    entities13e should have size (1)
    val text = entities13e.head.text
    text should be("climate change")
  }

  "sent13f" should "extract Climate entity mention" in {
    val mentions13f = extractMentions(sent13f)
    val entities13f = mentions13f.filter(_ matches CLIMATE)
    entities13f should have size (1)
    val text = entities13f.head.text
    text should be("Climate change")
  }

  "sent13g" should "extract 2 Climate entity mentions" in {
    val mentions13g = extractMentions(sent13g)
    val entities13g = mentions13g.filter(_ matches CLIMATE)
    entities13g should have size (2)
    val text = entities13g.map(_.text)
    text should contain("the climatic stresses")
    text should contain("climate change impacts")
  }

  "sent13h" should "extract Climate entity mention" in {
    val mentions13h = extractMentions(sent13h)
    val entities13h = mentions13h.filter(_ matches CLIMATE) //climate change
    entities13h should have size (1)
    val text = entities13h.head.text
    text should be("climate change impacts")
  }

  //TODO: unsure if it should extract this?
  "sent13i" should "extract Climate entity mention" in {
    val mentions13i = extractMentions(sent13i)
    val entities13i = mentions13i.filter(_ matches CLIMATE)
    entities13i should have size (1)
    val text = entities13i.head.text
    text should be("climate")
  }

  "sent13j" should "extract Climate entity mention" in {
    val mentions13j = extractMentions(sent13j)
    val entities13j = mentions13j.filter(_ matches CLIMATE) //climate change
    entities13j should have size (1)
    val text = entities13j.head.text
    text should be("climate change")
  }

  "sent13k" should "extract Climate entity mention" in  {
    val mentions13k = extractMentions(sent13k)
    val entities13k = mentions13k.filter(_ matches CLIMATE) //climate
    entities13k should have size (1)
    val text = entities13k.head.text
    text should be("the climate")
  }



  //Poverty
  val sent14a = "Significant decline in poverty will be associated with a decrease in family size and increase in non-farm income."
  val sent14b = "Poverty levels continue to increase, people become more vulnerable to food insecurity and other risks."
  val sent14c = "With increases in poverty levels people become more vulnerable to climate change and other risks."

  "sent14a" should "extract Poverty entity mention" in {
    val mentions14a = extractMentions(sent14a)
    val entities14a = mentions14a.filter(_ matches POVERTY) //poverty
    entities14a should have size (1)
    val text = entities14a.head.text
    text should be("poverty")
  }

  "sent14b" should "extract Poverty entity mention" in {
    val mentions14b = extractMentions(sent14b)
    val entities14b = mentions14b.filter(_ matches POVERTY) //Poverty levels
    entities14b should have size (1)
    val text = entities14b.head.text
    text should be("Poverty levels")
  }

  "sent14c" should "extract Poverty entity mention" in {
    val mentions14c = extractMentions(sent14c)
    val entities14c = mentions14c.filter(_ matches POVERTY) //poverty levels
    entities14c should have size (1)
    val text = entities14c.head.text
    text should be("poverty levels")
  }

  //Economy
  val sent15a = "Share of agriculture in overall economy will decrease with increase in inequality."
  val sent15b = "Government policy objective is to achieve food security, ensure adequate raw materials for the manufacturing sector and increased export earnings through increased productivity, efficient input use, improved investment and market access, infrastructure, and service development, targeting annual agricultural growth of 9.1% by 2015."
  val sent15c = "In agriculture, government promotes market-oriented production."
  val sent15d = "Farmers produce beyond subsistence, but fail to access profitable markets (inputs and outputs)."
  val sent15e = "Better and well-functioning agricultural credit and market services for both established and emerging farmers."
  val sent15f = "Farm size and wage rates will increase."
  val sent15g = "This requires more opportunities in non-agricultural income and increased technological interventions."

  "sent15a" should "extract Economy entity mention" in {
    val mentions15a = extractMentions(sent15a)
    val entities15a = mentions15a.filter(_ matches ECON) // economy
    entities15a should have size (1)
    val text = entities15a.head.text
    text should be("overall economy")
  }

  "sent15b" should "extract Economy entity mention" in {
    val mentions15b = extractMentions(sent15b)
    val entities15b = mentions15b.filter(_ matches ECON) //market access
    entities15b should have size (1)
    val text = entities15b.head.text
    text should be("market access")
  }

  //TODO: unsure if it should exact this or not
  "sent15c" should "extract Economy entity mention" in {
    val mentions15c = extractMentions(sent15c)
    val entities15c = mentions15c.filter(_ matches ECON) //market-oriented
    entities15c should have size (1)
    val text = entities15c.head.text
    text should be("market-oriented")
  }

  "sent15d" should "extract Economy entity mention" in {
    val mentions15d = extractMentions(sent15d)
    val entities15d = mentions15d.filter(_ matches ECON) //profitable markets
    entities15d should have size (1)
    val text = entities15d.head.text
    text should be("profitable markets")
  }

  "sent15e" should "extract Economy entity mention" in {
    val mentions15e = extractMentions(sent15e)
    val entities15e = mentions15e.filter(_ matches ECON) //market services
    entities15e should have size (1)
    val text = entities15e.head.text
    text should be("market services")
  }

  "sent15f" should "extract Economy entity mention" in {
    val mentions15f = extractMentions(sent15f)
    val entities15f = mentions15f.filter(_ matches ECON) // wage rates
    entities15f should have size (1)
    val text = entities15f.head.text
    text should be("wage rates")
  }

  "sent15g" should "extract Economy entity mention" in {
    val mentions15g = extractMentions(sent15g)
    val entities15g = mentions15g.filter(_ matches ECON) // wage rates
    entities15g should have size (1)
    val text = entities15g.head.text
    text should be("opportunities in non-agricultural income")
  }


  //Crop
  val sent16a = "There will not be significant changes in food imports, while yield of important crops will increase due to technological progress in agriculture."
  val sent16b = "Poor road construction and maintenance restrict private-sector investments in these high-potential agricultural areas (for crops and livestock)."
  val sent16c = "Climate change has an adverse impact on agricultural production system in the Indo-Gangetic region where rice–wheat is the predominant cropping system, which contributes to national food security."
  val sent16d = "Hence, government liberalizes imports of food grains, invests in food chain logistics, and boost research and development for new crop cultivars to boost agricultural production for ensuring food security."
  val sent16e = "There will be a small increase in crop diversity due to the need to combat the climate and market risks as both of these might become more volatile in the future."
  val sent16f = "Global trends suggest that rice–wheat production in the region will be adversely affected by climate change."

  "sent16a" should "extract Crop entity mention" in {
    val mentions16a = extractMentions(sent16a)
    val entities16a = mentions16a.filter(_ matches CROP) //important crops
    entities16a should have size (1)
    val text = entities16a.head.text
    text should be("yield of important crops")
  }

  "sent16b" should "extract Crop entity mention" in {
    val mentions16b = extractMentions(sent16b)
    val entities16b = mentions16b.filter(_ matches CROP) //crops
    entities16b should have size (1)
    val text = entities16b.head.text
    text should be("crops")
  }

  "sent16c" should "extract 2 Crop entity mentions" in {
    val mentions16c = extractMentions(sent16c)
    val entities16c = mentions16c.filter(_ matches CROP) //rice & predominant cropping system
    entities16c should have size (2)
    val text = entities16c.map(_.text)
    text should contain("rice")
    text should contain("the predominant cropping system")
  }

  "sent16d" should "extract Crop entity mention" in {
    val mentions16d = extractMentions(sent16d)
    val entities16d = mentions16d.filter(_ matches CROP) //new crop cultivars
    entities16d should have size (1)
    val text = entities16d.head.text
    text should be("new crop cultivars")
  }

  "sent16e" should "extract Crop entity mention" in {
    val mentions16e = extractMentions(sent16e)
    val entities16e = mentions16e.filter(_ matches CROP) //crop diversity
    entities16e should have size (1)
    val text = entities16e.head.text
    text should be("crop diversity")
  }

  "sent16f" should "extract Crop entity mention" in {
    val mentions16f = extractMentions(sent16f)
    val entities16f = mentions16f.filter(_ matches CROP) //crop diversity
    entities16f should have size (1)
    val text = entities16f.head.text
    text should be("rice")
  }

  //Pollution
  val sent17a = "Water quality and water availability for agriculture will decrease due to pollution of water bodies, and competition for water from other sources, but water-use efficiency in agriculture will increase due to technological progress."
  val sent17b = "Soil quality will decline by a small-to-medium extent, due to pollution, and intensive cultivation will be caused by a shrinking land base for agriculture."

  "sent17a" should "extract Pollution entity mention" in {
    val mentions17a = extractMentions(sent17a)
    val entities17a = mentions17a.filter(_ matches POLLUTION) //pollution of water bodies
    entities17a should have size (1)
    val text = entities17a.head.text
    text should be("pollution of water bodies")
  }

  "sent17b" should "extract Pollution entity mention" in {
    val mentions17b = extractMentions(sent17b)
    val entities17b = mentions17b.filter(_ matches POLLUTION) //pollution
    entities17b should have size (1)
    val text = entities17b.head.text
    text should be("pollution")
  }

  //Drought
  val sent18a = "The government promotes high-yielding and drought-/flood-tolerant rice varieties with policy to encourage the application of organic fertilizers, decreasing the cost on inorganic fertilizers."
  val sent18b = "Government puts more emphasis on improving the agricultural water irrigation/management system to cope with drought conditions."

  "sent18a" should "extract Drought entity mention" in {
    val mentions18a = extractMentions(sent18a)
    val entities18a = mentions18a.filter(_ matches DROUGHT) //drought-tolerant ???
    entities18a should have size (1)
    val text = entities18a.head.text
    text should be("drought")
  }

  "sent18b" should "extract Drought entity mention" in {
    val mentions18b = extractMentions(sent18b)
    val entities18b = mentions18b.filter(_ matches DROUGHT) //dought conditions
    entities18b should have size (1)
    val text = entities18b.head.text
    text should be("drought conditions")
  }


}
