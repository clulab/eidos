//package org.clulab.wm
//
//import org.scalatest._
//import TestUtils._
//import ReaderUtils._
//TODO: DEPRECIATED
///**
//  * Tests for Event Tests: Increase, Decrease, Remain, Cause-Effect
//  *
//  * Commented out sentences may have increase/decrease events, but don't have our Params and
//  * are thus excluded from testing at this time.
//  */
//
//class TestEvents extends FlatSpec with Matchers {
//
//  def msg(count: Int, event: String, sent: String) = s"extract $count $event EventMention from '$sent'"
//
//  //ONE Increase Event
//  val increase1 = "Better and well-functioning agricultural credit and market services for both established and emerging farmers."
//  val increase5 = "The support for agricultural research, education, and extension programs will also be increased for developing and disseminating climate change adaptation agricultural technologies to the farmers."
//  //val increase8 = "Trading land and human resources to foreign investors, who will in turn develop infrastructure."
//  //val increase11 = "Lack of competition input prices tend to be high, output prices generally low."
//  val increase12 = "Limited financial capacities and low education levels further restrict farmers’ ability for higher benefits from increased agricultural production."
//  //val increase18 = "The government aims to invest more in agriculture, shortage of labor with the consequence of decreased population growth and household size."
//  val increase19 = "The government promotes improved cultivar and climate-smart technologies but the policy to cut down the use of inorganic fertilizer and phase out the fertilizer subsidy results in deteriorating biophysical conditions, low use of inorganic fertilizer, less water, reduced farm sizes which lead to low benefit from the improved cultivar."
//  //val increase20 = "Corporate role in agriculture will increase with improved increase in commodity groups."
//  //val increase21 = "Increased uptake of adaptation strategies by commercial farmers."
//  val increase22 = "With increases in poverty levels people become more vulnerable to climate change and other risks."
//  //val increase24 = "Though the government adopts long-term and short-term policy measures, rice–wheat production costs increase substantially."
//  val increase25 = "There will be a small increase in crop diversity due to the need to combat the climate and market risks as both of these might become more volatile in the future."
//  //val increase27 = "Most subsidies are likely to decline while prices of agricultural commodities will increase."
//  //val increase30 = "Share of agriculture in overall economy will decrease with increase in inequality."
//  val increase31 = "Significant decline in poverty will be associated with a decrease in family size and increase in non-farm income."
//  val increase38 = "Poverty levels continue to increase, people become more vulnerable to food insecurity and other risks."
//  //increase39 "intensive cultivation" as an increase event? I took out "intensive" as a trigger
//  //val increase39 = "Soil quality will decline by a small-to-medium extent, due to pollution, and intensive cultivation will be caused by a shrinking land base for agriculture."
//  //This doesn't seem like a true increase either
//  //val increase41 = "Investment in infrastructure is, however, slow."
//  //val increase42 = "PEDSA (national strategic plan) will be funded by 2015 and various investment programs will be implemented."
//  val increase43 = "Government puts more emphasis on improving the agricultural water irrigation/management system to cope with drought conditions."
//  val increase45 = "Main interventions will include support for the agricultural-service sector, fertilizer subsidies, and feeder roads (slow)."
//
//  "rule TestEvents_Increase1" should msg(1, "Increase", increase1) in {
//
//    val mentions1 = extractMentions(increase1)
//    val events1 = mentions1.filter(_ matches INCREASE) //Param: Economy (market)
//    //This would be 2 increase events if we counted "agricultural credit", however this is not a Param
//    events1 should have size (1)
//  }
//
//
//  //TODO: incorrectly says Param: Climate is increasing.
//  //TODO: not sure if we should be extracting anything actually
//  it should msg(1, "Increase", increase5) in {
//    val mentions5 = extractMentions(increase5)
//    val events5 = mentions5.filter(_ matches INCREASE) //Param: Productivity (i.e. "technologies)
//    //Problem: its saying "Climate change" is increasing as well... ruh roh...
//    //3 Non-params are "supported": research, education, & programs
//    events5 should have size (1)
//  }
//
//
//  //This sentence has an IncreaseEvent, but NOT regarding any of our Params
////  it should msg(1, "Increase", increase8) in {
////    val mentions8 = extractMentions(increase8)
////    val events8 = mentions8.filter(_ matches INCREASE) //
////    events8 should have size (1)
////  }
//
//  //No Params, also not a clear Increase/Decrease in this situation
////  it should msg(1,  "Increase",increase11) in {
////    val mentions11 = extractMentions(increase11)
////    val events11 = mentions11.filter(_ matches INCREASE) //
////    events11 should have size (1)
////  }
//
//  it should msg(1,  "Increase", increase12) in {
//    val mentions12 = extractMentions(increase12)
//    val events12 = mentions12.filter(_ matches INCREASE) //
//    events12 should have size (1)
//  }
//
//
//  //"Use of improved cultivars will further decline" --> "improved cultivars" doesn't count (not Param)
////  it should msg(1, "Increase", increase15) in {
////    val mentions15 = extractMentions(increase15)
////    val events15 = mentions15.filter(_ matches INCREASE) //
////    events15 should have size (1)
////  }
//
//  //No params increase but "invest in agriculture" could be an Increase event.
////  "rule TestEvents_Increase1" should msg(1,  "Increase", increase18) in {
////    val mentions18 = extractMentions(increase18)
////    val events18 = mentions18.filter(_ matches INCREASE) // TWO -- invest in agriculture
////    events18 should have size (1)
////  }
//
//  //TODO: SHOULD only extract that technologies increase (trigger: promote)
//  //Incorrectly says that government promotes is an Increase Event (not a problem if Govmt removed as param)
//  //Incorrectly says "climate-smart" (ClimateEntity) increases (trigger: promote)
//  it should msg(1, "Increase", increase19) in {
//    val mentions19 = extractMentions(increase19)
//    val events19 = mentions19.filter(_ matches INCREASE) //Param: technologies
//    events19 should have size (1)
//  }
//
//  //No Params
////  it should msg(1, "Increase", increase20) in {
////    val mentions20 = extractMentions(increase20)
////    val events20 = mentions20.filter(_ matches INCREASE) //
////    events20 should have size (1)
////  }
//
//  //No Params
////  it should msg(1, "Increase", increase21) in {
////    val mentions21 = extractMentions(increase21)
////    val events21 = mentions21.filter(_ matches INCREASE) //
////    events21 should have size (1)
////  }
//
//  it should msg(1,  "Increase", increase22) in {
//    val mentions22 = extractMentions(increase22)
//    val events22 = mentions22.filter(_ matches INCREASE) // Increase22 has increase x2: "increase in poverty"
//    events22 should have size (1)
//  }
//
//  //No Params increase
////  it should msg(1, "Increase", increase24) in {
////    val mentions24 = extractMentions(increase24)
////    val events24 = mentions24.filter(_ matches INCREASE) //
////    events24 should have size (1)
////  }
//
//
//  // FIXED!!!!
//  // Was failing to get increase because "increase" gets wrapped up in Param!!!!
//  /**
//    * Rule => simple_entity_1-copyLabel
//	Type => TextBoundMention
//	------------------------------
//	Crop, Param, Entity, Potential_Cause => increase in crop diversity due to the need to
//    * */
//  it should msg(1, "Increase", increase25) in {
//    val mentions25 = extractMentions(increase25)
//    val events25 = mentions25.filter(_ matches INCREASE) //
//    events25 should have size (1)
//  }
//
//  //"Commodities" is not a Param
////  it should msg(1, "Increase", increase27) in {
////    val mentions27 = extractMentions(increase27)
////    val events27 = mentions27.filter(_ matches INCREASE) //
////    events27 should have size (1)
////  }
//
//  //Inequality is not a Param
////  it should msg(1, "Increase", increase30) in {
////    val mentions30 = extractMentions(increase30)
////    val events30 = mentions30.filter(_ matches INCREASE) //
////    events30 should have size (1)
////  }
//
//  it should msg(1, "Increase", increase31) in {
//    val mentions31 = extractMentions(increase31)
//    val events31 = mentions31.filter(_ matches INCREASE) //
//    events31 should have size (1)
//  }
//
//
//  it should msg(1, "Increase", increase38) in {
//    val mentions38 = extractMentions(increase38)
//    val events38 = mentions38.filter(_ matches INCREASE) //
//    events38 should have size (1)
//  }
//
//  //No params increase, and also "intensive" is a bad trigger
////  it should msg(1, "Increase", increase39) in {
////    val mentions39 = extractMentions(increase39)
////    val events39 = mentions39.filter(_ matches INCREASE) //
////    events39 should have size (1)
////  }
//
//  //"Investment in blah" should be an Entity but its not really Increasing here.
////  it should msg(1, "Increase", increase41) in {
////    val mentions41 = extractMentions(increase41)
////    val events41 = mentions41.filter(_ matches INCREASE) //
////    events41 should have size (1)
////  }
//
//  //No Params truly increasing here. "Investment" not necessarily indicative of increasing?
//  //Also would be just 1 IncreaseEvent Anyway
//  //  it should msg(1, "Increase", increase42) in {
//  //    val mentions42 = extractMentions(increase42)
//  //    val events42 = mentions42.filter(_ matches INCREASE)
//  //    events42 should have size (1)
//  //
//  //  }
//
//  it should msg(1, "Increase", increase43) in {
//    val mentions43 = extractMentions(increase43)
//    val events43 = mentions43.filter(_ matches INCREASE) //
//    events43 should have size (1)
//
//  }
//
//  //Only Param increasing is "fertilizer subsidies"
//  it should msg(1,  "Increase", increase45) in {
//    val mentions45 = extractMentions(increase45)
//    val events45 = mentions45.filter(_ matches INCREASE) //3 increase events. "support X1, X2, X3"
//    events45 should have size (1)
//
//  }
//
//
//  // TWO Increase Events
//  // val increase6 = "PNISA (strategy area in PEDSA, National Investment for Agriculture) will define the requirements for developing the agricultural sector (public/private)."
//  // val increase7 = "Private-sector development will be through CEPAGR, infrastructure development through PROIRRI."
//  //val increase17 = "The government aims to improve food security through self-sufficiency in rice with a framework to promote the rice sector to cope with impacts of variable climate."
//  val increase13 = "The government promotes high-yielding and drought-/flood-tolerant rice varieties with policy to encourage the application of organic fertilizers, decreasing the cost on inorganic fertilizers."
//  val increase14 = "Use of improved cultivars and mechanization will be increased and use of critical interventions may lead to increases in productivity and efficient use of resources."
//  val increase23 = "Therefore, the government is committed to supporting the agriculture sector through increased public investment to fulfill the needs of an increasing population."
//  val increase26 = "but water-use efficiency in agriculture will increase due to technological progress."
//  val increase28 = "Farm size and wage rates will increase."
//  val increase29 = "Mechanization and energy-use intensity in agriculture will increase."
//  val increase33 = "Fertilizer-use intensity and fertilizer productivity will increase."
//  val increase32 = "There will not be significant changes in food imports, while yield of important crops will increase due to technological progress in agriculture."
//  val increase35 = "This requires more opportunities in non-agricultural income and increased technological interventions."
//  val increase37 = "However, opportunities for massive increases in agricultural production and productivity exist but are not being exploited."
//
//
//    //No params and no real increase events...
//  //  "rule TestEvents_Increase2" should msg(2,  "Increase", increase6) in {
//  //      val mentions6 = extractMentions(increase6)
//  //      val events6 = mentions6.filter(_ matches INCREASE)
//  //      events6 should have size (2)
//  //  }
//
//    //No Params
//  //  "rule TestEvents_Increase2"  should msg(2,  "Increase", increase7) in {
//  //    val mentions7 = extractMentions(increase7)
//  //    val events7 = mentions7.filter(_ matches INCREASE) // increase7 has 2x "development"
//  //    events7 should have size (2)
//  //  }
//
//  //incorrectly says drought increases and cost on inorganic fertilizers Increase
//  //crop increase & fert-use increase
//  "rule TestEvents_Increase2" should msg(2,  "Increase", increase13) in {
//    val mentions13 = extractMentions(increase13)
//    val events13 = mentions13.filter(_ matches INCREASE) //
//    events13 should have size (2)
//  }
//
//
//  //TODO: only gets 1 Productivy Increase :'(
//  it should msg(2, "Increase", increase14) in {
//    val mentions14 = extractMentions(increase14)
//    val events14 = mentions14.filter(_ matches INCREASE) //Params: Productivity 2x
//    events14 should have size (2)
//  }
//  //No Params. Because "food security" & "rice sector" are not Params
////  it should msg(2, "Increase",  increase17) in {
////    val mentions17 = extractMentions(increase17)
////    val events17 = mentions17.filter(_ matches INCREASE) // increase 17 has 2: increase & promote
////    events17 should have size (2)
////  }
//
//
//
//  //Params: Increased Investment and Increase Population
//  //TODO: incorrectly gets "public investment to" 2x with Increase_ported_syntax_3_verb rule with triggers "increase" and "supporting"
//  it should msg(2, "Increase", increase23) in {
//    val mentions23 = extractMentions(increase23)
//    val events23 = mentions23.filter(_ matches INCREASE) //increase 23 has increase x2
//    events23 should have size (2)
//  }
//
//
//  it should msg(2, "Increase", increase26) in {
//    val mentions26 = extractMentions(increase26)
//    val events26 = mentions26.filter(_ matches INCREASE) //increase 26 has two "water-use efficiency will increase" & "technological progress"
//    events26 should have size (2)
//  }
//
//  it should msg(2, "Increase", increase28) in {
//    val mentions28 = extractMentions(increase28)
//    val events28 = mentions28.filter(_ matches INCREASE) //
//    events28 should have size (2)
//  }
//
//  it should msg(2, "Increase", increase29) in {
//    val mentions29 = extractMentions(increase29)
//    val events29 = mentions29.filter(_ matches INCREASE) //Params: Productivity x2: Mechanization & Energy-Use
//    events29 should have size (2)
//  }
//
//  it should msg(1, "Increase", increase32) in {
//    val mentions32 = extractMentions(increase32)
//    val events32 = mentions32.filter(_ matches INCREASE) //increase32 has 2: "crops increase" & "tech progress"
//    events32 should have size (2)
//  }
//
//
//  it should msg(2, "Increase", increase33) in {
//    val mentions33 = extractMentions(increase33)
//    val events33 = mentions33.filter(_ matches INCREASE) //
//    events33 should have size (2)
//  }
//
//
//  it should msg(2, "Increase", increase35) in {
//    val mentions35 = extractMentions(increase35)
//    val events35 = mentions35.filter(_ matches INCREASE) //
//    events35 should have size (2)
//  }
//
//  it should msg(2, "Increase", increase37) in {
//    val mentions37 = extractMentions(increase37)
//    val events37 = mentions37.filter(_ matches INCREASE) //Productivity x2
//    events37 should have size (2)
//  }
//
//
//
//  // THREE Increase Events per sentence
//  val increase2 = "The governmental policy objective is to achieve food security, ensure adequate raw materials for the manufacturing sector, and increased export earnings through increased productivity, efficient input use, and better market access, infrastructure, and service development."
//  val increase3 = "Hence, government liberalizes imports of food grains, invests in food chain logistics, and boost research and development for new crop cultivars to boost agricultural production for ensuring food security."
//  //val increase40 = "Government and state policies invest in extractive industries, also with an aim to uplift agriculture and food security."
//
//  //Increase Params: Economy(market access), Productivity (productivity)
//  "rrule TestEvents_Increase3" should msg(3, "Increase", increase2) in {
//    val mentions2 = extractMentions(increase2)
//    val events2 = mentions2.filter(_ matches INCREASE) //
//    events2 should have size (3)
//  }
//
//
//  //TODO: Incorrectly says "Government" is increasing 2x via triggers "invest" & "boost" via Increase-verb-1
//  //Wondering if maybe "government" is a bad Param to have...
//  it should msg(3, "Increase", increase3) in {
//    val mentions3 = extractMentions(increase3)
//    val events3 = mentions3.filter(_ matches INCREASE) //Params: Productivity x2 (logistics and productivity), Crop x1 (crop cultivars)
//    events3 should have size (3)
//  }
//
//
//  //No Params are actually increasing
////  it should msg(3, "Increase", increase40) in {
////    val mentions40 = extractMentions(increase40)
////    val events40 = mentions40.filter(_ matches INCREASE) //increase40 has invest IncreaseEvent and uplift agrictulture & food security IncreaseEvents (3 total)
////    events40 should have size (3)
////  }
//
//  //FOUR Incrase events
//  val increase16 = "A combination of increasing population, government plans to invest in fertilizer factory, government subsidy on fertilizers, improved economic performance expected to cause a shift from agriculture to service industry, government plans for massive expansion of irrigation (irrigate 1 million ha.), newly devolved county governments etc. are some of the developments expected to change agriculture development in the country."
//
//  //Params increasing: population, fertilizer-SOMETHING, economic performance (economy), irrigation
//  // maybe actually 5 with increased government
//  "rule TestEvents_Increase4" should msg(4, "Increase", increase16) in {
//    val mentions16 = extractMentions(increase16)
//    val events16 = mentions16.filter(_ matches INCREASE) //increase 16 should have 5 increase events (population, invest, economic performance, expansion of irrigation, developed county govmts)
//    events16 should have size (4)
//  }
//
//
//
//
//  //FIVE Increase Events
//  val increase4 = "Along with the support programs such as agricultural insurance and input subsidies, the government efforts and investments will be increased for extending irrigation services, agricultural mechanization, and developing disaster risk-management practices."
//
//
//  "rule TestEvents_Increase5" should msg(5,  "Increase", increase4) in {
//    val mentions4 = extractMentions(increase4)
//    val events4 = mentions4.filter(_ matches INCREASE)
//    //5 Increase params: Subsidy(subsidies), Government(the govmt efforts), Economy(investments), Water(irrigation), & Productivity (mechanization);
//    events4 should have size (5)
//  }
//
//
//
//
//  //NEGATED Increases:
//
//  //HINDERS development, LACK of incentives for investment
//  val outlier1 = "Persisting economic crisis, governments extractive policies (high taxes), and lack of incentives and security for private-sector investment hinder development."
//  //LIMIT development
//  val outlier2 = "Underfunded public sector and underperformance of the private sector limit development of the agricultural sector and result in unsustainable import bills for agricultural commodities."
//  //major barrier to development
//  val outlier3 = "Poor infrastructure is a major barrier to agricultural development."
//  //lack of clarity for investment
//  val outlier6 = "The transformation however starts under extremely difficult conditions, characterized by large account deficit and liquidity challenges and limited direct foreign investment due to lack of clarity on investment security and high interest rates."
//  //RESTRICTED investment
//  val outlier7 = "Poor road construction and maintenance restrict private-sector investments in these high-potential agricultural areas (for crops and livestock)."
//
//
//  "rule TestEvents_Not_Increase" should msg(0, "Increase", outlier1) in {
//    val mentionsOut1 = extractMentions(outlier1)
//    val eventsOut1 = mentionsOut1.filter(_ matches INCREASE)
//    eventsOut1 shouldBe empty
//  }
//
//  it should msg(0,  "Increase", outlier2) in {
//    val mentionsOut2 = extractMentions(outlier2)
//    val eventsOut2 = mentionsOut2.filter(_ matches INCREASE)
//    eventsOut2 shouldBe empty
//  }
//
//  it should msg(0,  "Increase", outlier3) in {
//    val mentionsOut3 = extractMentions(outlier3)
//    val eventsOut3 = mentionsOut3.filter(_ matches INCREASE)
//    eventsOut3 shouldBe empty
//  }
//
//
//  it should msg(0, "Increase", outlier6) in {
//    val mentionsOut6 = extractMentions(outlier6)
//    val eventsOut6 = mentionsOut6.filter(_ matches INCREASE)
//    eventsOut6 shouldBe empty
//  }
//
//  it should msg(0, "Increase", outlier7) in {
//    val mentionsOut7 = extractMentions(outlier7)
//    val eventsOut7 = mentionsOut7.filter(_ matches INCREASE)
//    eventsOut7 shouldBe empty
//  }
//
//
//
//  ///////////////////////////////////////////////////////////////////////////////////////
//
//  //Decrease
//
//
//  // ONE DecreaseEvent
//  val decrease1 = "Global trends suggest that rice–wheat production in the region will be adversely affected by climate change."
//  val decrease4 = "Most subsidies are likely to decline while prices of agricultural commodities will increase."
//  //val decrease6 = "Use of improved cultivars will force further decline."
//  //val decrease7 = "Share of agriculture in overall economy will decrease with increase in inequality."
//  //val decrease13 = "Lack of competition input prices tend to be high, output prices generally low."
//  val decrease14 = "Limited financial capacities and low education levels further restrict farmers’ ability for higher benefits from increased agricultural production."
// // val decrease15 = "However, the government will prioritize its programs to minimize the loss from climate change impacts and reduce the vulnerability of the people. "
// // val decrease16 = "This will support them as they adapt to climate change and reduce their vulnerability."
// val decrease17 = "With a high cost of production and degraded natural resources, profitability in agriculture may be further reduced, making agriculture unprofitable."
//  val decrease18 = "Labor migration to urban areas, non-agricultural activities and impact of HIV/AIDS also leads to labor shortages."
//
//
//  //TODO: incorrectly says Climate decreases ("affected by climate change")
//  "rule TestEvents_Decrease1" should msg(1, "Decrease", decrease1) in {
//    val mentionsd1 = extractMentions(decrease1)
//    val eventsd1 = mentionsd1.filter(_ matches DECREASE) //Param: Productivity decreases
//    eventsd1 should have size (1)
//  }
//
//  it should msg(1, "Decrease", decrease4) in {
//    val mentionsd4 = extractMentions(decrease4)
//    val eventsd4 = mentionsd4.filter(_ matches DECREASE)
//    eventsd4 should have size (1)
//  }
//
//  //"Improved cultivars" is not a Param, therefore this is not an Decrease Event for us
////  it should msg(1, "Decrease", decrease6) in {
////    val mentionsd6 = extractMentions(decrease6)
////    val eventsd6 = mentionsd6.filter(_ matches DECREASE)
////    eventsd6 should have size (1)
////  }
//
//  //"Share of agriculture" is not a Param
////  it should msg(1, "Decrease", decrease7) in {
////    val mentionsd7 = extractMentions(decrease7)
////    val eventsd7 = mentionsd7.filter(_ matches DECREASE)
////    eventsd7 should have size (1)
////  }
//
//  // "Prices low" doesn't seem like an obvious Decrease to me
////  it should msg(1, "Decrease", decrease13) in {
////    val mentionsd13 = extractMentions(decrease13)
////    val eventsd13 = mentionsd13.filter(_ matches DECREASE)
////    eventsd13 should have size (1)
////  }
//
//  //TODO: Should "limited financial capacities" be counted as "Decrease in Economy?"
//  it should msg(1, "Decrease", decrease14) in {
//    val mentionsd14 = extractMentions(decrease14)
//    val eventsd14 = mentionsd14.filter(_ matches DECREASE)
//    eventsd14 should have size (1)
//  }
//
//  //"loss" and/or "vulnerability" are not Params
////  it should msg(1, "Decrease", decrease15) in {
////    val mentionsd15 = extractMentions(decrease15)
////    val eventsd15 = mentionsd15.filter(_ matches DECREASE)
////    eventsd15 should have size (1)
////  }
//
//  //Vulnerability not a Param
////  it should msg(1, "Decrease", decrease16) in {
////    val mentionsd16 = extractMentions(decrease16)
////    val eventsd16 = mentionsd16.filter(_ matches DECREASE)
////    eventsd16 should have size (1)
////  }
//
//  it should msg(1, "Decrease", decrease17) in {
//    val mentionsd17 = extractMentions(decrease17)
//    val eventsd17 = mentionsd17.filter(_ matches DECREASE) //proftability (Economy) decreases
//    eventsd17 should have size (1)
//
//  }
//
//  it should msg(1, "Decrease", decrease18) in {
//    val mentionsd18 = extractMentions(decrease18)
//    val eventsd18 = mentionsd18.filter(_ matches DECREASE)
//    eventsd18 should have size (1)
//
//  }
//
//
//  // TWO DecreaseEvents
//  val decrease5 = "Significant decline in poverty will be associated with a decrease in family size and increase in non-farm income."
//  val decrease9 = "Water quality and water availability for agriculture will decrease due to pollution of water bodies, and competition for water from other sources,"
//  val decrease19 = "Agricultural production and profitability are declining, land is degrading and being underutilized."
//
//  "rule TestEvents_Decrease2" should msg(2, "Decrease", decrease5) in {
//    val mentionsd5 = extractMentions(decrease5)
//    val eventsd5 = mentionsd5.filter(_ matches DECREASE) //2 decreaseEvents: decline in poverty & decrease in family size
//    eventsd5 should have size (2)
//  }
//
//  //TODO: incorrectly says "Pollution" decreases and "competition for water from other sources" decreases
//  it should msg(2, "Decrease", decrease9) in {
//    val mentionsd9 = extractMentions(decrease9)
//    val eventsd9 = mentionsd9.filter(_ matches DECREASE) //2 decreaseEvents: water quality, water avaliablity
//    eventsd9 should have size (2)
//  }
//
//  it should msg(2, "Decrease", decrease19) in {
//    val mentionsd19 = extractMentions(decrease19)
//    val eventsd19 = mentionsd19.filter(_ matches DECREASE) //Params: Productivity & Economy (profitability)
//    eventsd19 should have size (2)
//
//  }
//
//
//  //THREE DecreaseEvents
//  val decrease3 = "Soil quality will decline by a small-to-medium extent, due to pollution, and intensive cultivation will be caused by a shrinking land base for agriculture."
//  val decrease8 = "The government aims to invest more in agriculture, shortage of labor with the consequence of decreased population growth and household size."
//
//  "rule TestEvents_Decrease3" should msg(3, "Decrease", decrease3) in {
//    val mentionsd8 = extractMentions(decrease8)
//    val eventsd8 = mentionsd8.filter(_ matches DECREASE) //3 decreaseEvents: shortage of labor, decrease population & decrease household,
//    eventsd8 should have size (3)
//  }
//
//
//  //FIVE DecreaseEvents
//  val decrease2 = "the policy to cut down the use of inorganic fertilizer and phase out the fertilizer subsidy results in deteriorating biophysical conditions, low use of inorganic fertilizer, less water, reduced farm sizes which lead to low benefit from the improved cultivar."
//
//  //TODO: incorrectly gets FertilizerUse decreasing 3x
//  "rule TestEvents_Decrease5" should msg(5, "Decrease", decrease2) in {
//    val mentionsd2 = extractMentions(decrease2)
//    val eventsd2 = mentionsd2.filter(_ matches DECREASE)
//    //Params: cut FertilizerUse & cut Subsidy; reduced Fertilizer Use, less Water, reduced Farmsize,
//    eventsd2 should have size (5)
//
//  }
//
//
//
//  ///////////////////////////////////////////////////////////////////////////////////////
//
//  //Remain remain|stay
//  //remainEvent: people remaine in rural areas
//  //val remain1 = "Most people remain in rural areas where agriculture is the main livelihood activity due to lack of alternatives."
//  val remain2 = "Climate change remains as a key challenge for a country like Nepal where subsistence-based and rainfed agriculture system is dominant."
//  val remain3 = "Most people remain in rural areas where agriculture is the main livelihood activity due to lack of alternatives."
//  //No params here
////  "rule TestEvents_Remain1" should msg(1, "Remain", remain1) in {
////    val mentionsr1 = extractMentions(remain1)
////    val eventsr1 = mentionsr1.filter(_ matches REMAIN)
////    eventsr1 should have size (1)
////  }
//
//  "rule TestEvents_Remain1"  should msg(1,"Remain", remain2) in {
//    val mentionsr2 = extractMentions(remain2)
//    val eventsr2 = mentionsr2.filter(_ matches REMAIN)
//    eventsr2 should have size (1)
//
//  }
//
//    //people (household size remains)
//  it should msg(1,"Remain", remain3) in {
//    val mentionsr3 = extractMentions(remain3)
//    val eventsr3 = mentionsr3.filter(_ matches REMAIN)
//    eventsr3 should have size (1)
//
//  }
//
//
//  ///////////////////////////////////////////////////////////////////////////////////////
//
//  //Cause-Effect Entities
//  //If the effect is NOT an Event(Increase, Decrease, or Remain), the rule will NOT be triggered!
//
//  //ONE CAUSE-EFFECT EVENT
//
//
//  //ce1 NEGATIVE "there will not be changed"
//  //ce1 effect: crops will increase; cause: tech
//  //TODO: "while yeild of crops will increase(EFFECT) due to tech progress(CAUSE)"
//  //TODO: "due to" has
//
//
////  16: (10,mark) (11,nsubj) (15,aux) (17,acomp)
////  17: (20,prep_to)
////&
////  16: (5,advcl)
////  17: (16,acomp)
//  val ce1 = "There will not be significant changes in food imports, while yield of important crops will increase due to technological progress in agriculture."
//
//  //causes: population, govmt plans, subsidy, improved economic performance (4 causes!)
//  //effect: a shift
//  //TODO: this one wont be captured because the CAUSES are Events, but not the EFFECT.
//  val ce2 = "A combination of increasing population, government plans to invest in fertilizer factory, government subsidy on fertilizers, improved economic performance expected to cause a shift from agriculture to service industry, government plans for massive expansion of irrigation (irrigate 1 million ha.), newly devolved county governments etc. are some of the developments expected to change agriculture development in the country."
//
//  //cause: need to combat climate change & market risks (2 causes!!)
//  //effect: increase in crop diversity
//  //TODO: Problem: "increase" is being caught in Crop mention.... why.... :'(
//  //TODO; "due to" have NOTHING on dependency graph
//  val ce4 = "There will be a small increase in crop diversity due to the need to combat the climate and market risks as both of these might become more volatile in the future."
//
//  //cause: pollution & competition (2 effects!)
//  //effect: water quality & water avaliability (2 effects!)
//  //TODO: the "due to" rule is very weak
//  val ce5a = "Water quality and water availability for agriculture will decrease due to pollution of water bodies, and competition for water from other sources,"
//
//  //cause: tech progress
//  //effect:water-use efficiency increases
//  val ce5b = "but water-use efficiency in agriculture will increase due to technological progress."
//
//  //effect: deficit & limited investment (2 causes)
//  //cause: lack of clarity & high interest rates (2 effects)
//  val ce6 = "The transformation however starts under extremely difficult conditions, characterized by large account deficit and liquidity challenges and limited direct foreign investment due to lack of clarity on investment security and high interest rates."
//
//  //cause: labor migration & HIV/AIDS (2 causes)
//  //effect: labor shortage
//  val ce7 = "Labor migration and HIV/AIDS result in labor shortage."
//
//  //cause: public sector & private sector OR just the "limit development" ??
//  //effect: unsustainable import bills
//  val ce8 = "Underfunded public sector and underperformance of the private sector limit development of the agricultural sector and result in unsustainable import bills for agricultural commodities."
//
//  //cause: increase in inequality
//  //effect: economy decreases
//  val ce10 = "Share of agriculture in overall economy will decrease with increase in inequality."
//
//  //cause: decrease in family size & increased income (2 causes!)
//  //effect: decline in poverty
//  val ce11 = "Significant decline in poverty will be associated with a decrease in family size and increase in non-farm income."
//
//  //cause: lack of alternatives
//  //effect: people remaine in rural areas
//  val ce12 = "Most people remain in rural areas where agriculture is the main livelihood activity due to lack of alternatives."
//
//  //cause: anticipated losses
//  //effect: adoption process instigated
//  val ce13 = "The adoption process will be instigated due to the anticipated losses in agricultural productivity in the face of climatic uncertainties."
//
//
//  //Problem: "due to" is getting caught in the Productivity mention
//  "rule TestEvents_CauseEffect1" should msg(1, "CauseEffect", ce1) in {
//    val mentionsCe1 = extractMentions(ce1)
//    val eventsCe1 = mentionsCe1.filter(_ matches CAUSEEF)
//    eventsCe1 should have size (1)
//  }
//
//  it should msg(1, "CauseEffect", ce2) in {
//    val mentionsCe2 = extractMentions(ce2)
//    val eventsCe2 = mentionsCe2.filter(_ matches CAUSEEF)
//    eventsCe2 should have size (1)
//  }
//
//  it should msg(1, "CauseEffect", ce4) in {
//    val mentionsCe4 = extractMentions(ce4)
//    val eventsCe4 = mentionsCe4.filter(_ matches CAUSEEF)
//    eventsCe4 should have size (1)
//  }
//
//  it should msg(1, "CauseEffect", ce5a) in {
//    val mentionsCe5a = extractMentions(ce5a)
//    val eventsCe5a = mentionsCe5a.filter(_ matches CAUSEEF)
//    eventsCe5a should have size (1)
//  }
//
//  it should msg(1, "CauseEffect", ce5b) in {
//    val mentionsCe5b = extractMentions(ce5b)
//    val eventsCe5b = mentionsCe5b.filter(_ matches CAUSEEF)
//    eventsCe5b should have size (1)
//  }
//
//  it should msg(1, "CauseEffect", ce6) in {
//    val mentionsCe6 = extractMentions(ce6)
//    val eventsCe6 = mentionsCe6.filter(_ matches CAUSEEF)
//    eventsCe6 should have size (1)
//  }
//
//  it should msg(1, "CauseEffect", ce7) in {
//    val mentionsCe7 = extractMentions(ce7)
//    val eventsCe7 = mentionsCe7.filter(_ matches CAUSEEF)
//    eventsCe7 should have size (1)
//  }
//
//  it should msg(1, "CauseEffect", ce8) in {
//    val mentionsCe8 = extractMentions(ce8)
//    val eventsCe8 = mentionsCe8.filter(_ matches CAUSEEF)
//    eventsCe8 should have size (1)
//  }
//
//  it should msg(1, "CauseEffect", ce10) in {
//    val mentionsCe10 = extractMentions(ce10)
//    val eventsCe10 = mentionsCe10.filter(_ matches CAUSEEF)
//    eventsCe10 should have size (1)
//  }
//
//  it should msg(1,"CauseEffect",  ce11) in {
//    val mentionsCe11 = extractMentions(ce11)
//    val eventsCe11 = mentionsCe11.filter(_ matches CAUSEEF)
//    eventsCe11 should have size (1)
//  }
//
//  it should msg(1, "CauseEffect", ce12) in {
//    val mentionsCe12 = extractMentions(ce12)
//    val eventsCe12 = mentionsCe12.filter(_ matches CAUSEEF)
//    eventsCe12 should have size (1)
//  }
//
//  it should msg(1, "CauseEffect", ce13) in {
//    val mentionsCe13 = extractMentions(ce13)
//    val eventsCe13 = mentionsCe13.filter(_ matches CAUSEEF)
//    eventsCe13 should have size (1)
//  }
//
//
//  //TWO Cause-EFFECT EVENTS
//
//  //ce3 has 2 cause-effect mentions
//  //causes1: pollution
//  //effect1: soil quality declines
//  //cause2: shrinking land base
//  //effect2: intensive cultivation
//  val ce3 = "Soil quality will decline by a small-to-medium extent, due to pollution, and intensive cultivation will be caused by a shrinking land base for agriculture."
//
//  //cause: the policy
//  //effect: deteriorating biophysical conditions, low use of inorganic fertilizer, less water, & reduced farm sizes
//  //&
//  //cause: deteriorating biophysical conditions, low use of inorganic fertilizer, less water, reduced farm sizes
//  //effect: low benefit
//  val ce9 = "The government promotes improved cultivar and climate-smart technologies but the policy to cut down the use of inorganic fertilizer and phase out the fertilizer subsidy results in deteriorating biophysical conditions, low use of inorganic fertilizer, less water, reduced farm sizes which lead to low benefit from the improved cultivar"
//
//  "rule TestEvents_CauseEffect2" should msg(2, "CauseEffect", ce3) in {
//    val mentionsCe3 = extractMentions(ce3)
//    val eventsCe3 = mentionsCe3.filter(_ matches CAUSEEF)
//    eventsCe3 should have size (2)
//  }
//
//  it should msg(2, "CauseEffect", ce9) in {
//    val mentionsCe9 = extractMentions(ce9)
//    val eventsCe9 = mentionsCe9.filter(_ matches CAUSEEF)
//    eventsCe9 should have size (2)
//  }
//
//
//  ///////////////////////////////////////////////////////////////////////////////////////
//  // UNSURE
//  //increase44 "prioritize its programs" an increaseEvent ????
//  val increase44 = "However, the government will prioritize its programs to minimize the loss from climate change impacts and reduce the vulnerability of the people."
//
//  //Not sure if "high cost of production" should be counted as an IncreaseEvent?? increase9 & 10
//  val increase9a = "With the high cost of production, food imports will further reduce farmers’ chances to make a living from agriculture."
//  val increase9b = "With a high cost of production and degraded natural resources, profitability in agriculture may be further reduced, making agriculture unprofitable."
//  val increase10 = "The transformation however starts under extremely difficult conditions, characterized by large account deficit and liquidity challenges and limited direct foreign investment due to lack of clarity on investment security and high interest rates."
//
//  //decrease in imports??
//  val decrease11 = "Imports are inadequate to meet domestic demand."
//  val decrease12 = "Incentives in the form of assured prices (minimum support prices) are inadequate to enhance agricultural production to meet food demand."
//
//
//
//}
