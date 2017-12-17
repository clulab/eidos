package org.clulab.wm

import org.scalatest._
import TestUtils._
import ReaderUtils._

class TestModifications extends FlatSpec with Matchers {


  //ONE Increase Event
  val sent1 = "Better and well-functioning agricultural credit and market services for both established and emerging farmers."

  sent1 should "have 1 IncreaseEvent for 'agricultural credit' " in {

    val mentions = extractMentions(sent1)

    println(s"SENT-1-Mentions: ${ mentions }")
    val events = mentions.filter(m => m.attachments.exists(a => a.isInstanceOf[Increase]))
    println(s"SENT-1-Events: ${ events }")

    events should have size (1)
    for (e <- events) {
      e.arguments.get("theme").get should have size (1) //Param: Economy
      e.arguments.get("theme").get.exists(_.text == "market services") should be(true)
      //      e.arguments.get("theme").get.exists(_.text.contains("market services")) should be (true)
    }

  }

  val sent2 = "The support for agricultural research, education, and extension programs will also be increased for developing and disseminating climate change adaptation agricultural technologies to the farmers."

  sent2 should "have 1 Increase Event for Productivity" in {
    val mentions = extractMentions(sent2)
    val events = mentions.filter(_ matches INCREASE) //Param: Productivity (i.e. "technologies)
    //3 Non-params are "supported": research, education, & programs
    events should have size (1)
    for (e <- events) {
      e.arguments.get("theme").get should have size (1) //Param: Productivty
      e.arguments.get("theme").get.exists(_.label == "Productivity")
      e.arguments.get("theme").get.exists(_.text == "climate change adaptation agricultural technologies") should be(true)
    }

  }


  //TODO: See sent3 test as example for how to test the eidos modified entities.
  //TODO: Update other tests accordingly
  //TODO: Move any cause-effect tests to a TestEvents class
  val sent3 = "Limited financial capacities and low education levels further restrict farmers’ ability for " +
    "higher benefits from increased agricultural production."
  // (DEC-limited) financial capacities
  // (QUANT-low) education levels
  // (INC-inc) agricultural production
  val mentions3 = extractMentions(sent3)

  sent3 should "have three modified entities" in {
    val entities = mentions3.count(_.attachments.nonEmpty) should be (3)
  }

  sent3 should "have one Increase modification" in {
    val entitiesWithInc = mentions3.filter(m => m.attachments.exists(a => a.isInstanceOf[Increase]))
    entitiesWithInc should have size (1)

    for (e <- entitiesWithInc) {
      e.attachments.head.asInstanceOf[Increase].trigger should be("increased")
      e.text should be("agricultural production")
    }
  }

  sent3 should "have one Quantification modification" in {
    val entities = mentions3.filter(m => m.attachments.exists(a => a.isInstanceOf[Quantification]))
    entities should have size (1)

    for (e <- entities) {
      e.attachments.head.asInstanceOf[Quantification].quantifier should be("low")
      e.text should be("education levels")
    }
  }

  sent3 should "have one Decrease modification" in {
    val entities = mentions3.filter(m => m.attachments.exists(a => a.isInstanceOf[Decrease]))
    entities should have size (1)

    for (e <- entities) {
      e.attachments.head.asInstanceOf[Decrease].trigger should be("restrict")
      e.text should be("education levels")
    }
  }

  //    val eventsDecrease = mentions.filter(_ matches DECREASE)
//    eventsDecrease should have size (1)
//
//    for (e <- eventsDecrease) {
//      e.arguments.get("theme").get should have size (1) //Param: Economy
//      e.arguments.get("theme").get.exists(_.label == "Economy")
//      e.arguments.get("theme").get.exists(_.text == "financial capacities") should be(true)
//    }




  val sent4: String = "The government promotes improved cultivar and climate-smart technologies but the policy to cut down the use of inorganic fertilizer and phase out the fertilizer subsidy results in deteriorating biophysical conditions, low use of inorganic fertilizer, less water, reduced farm sizes which lead to low benefit from the improved cultivar."

  val mentions4 = extractMentions(sent4)
  val eventsIncrease4 = mentions4.filter(_ matches INCREASE)
  val eventsDecrease4 = mentions4.filter(_ matches DECREASE)
  val eventsCause4 = mentions4.filter(_ matches CAUSEEF)

  // INCREASE EVENT:
  "sent4" should "have 1 Productivity increase event" in {
    eventsIncrease4 should have size (1)

    val filteredEvents = eventsIncrease4.filter(_.arguments("theme").head.label == "Productivity")
    filteredEvents should have size (1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("climate-smart technologies")
  }

  // DECREASE EVENTS:
  //Put down inorganic fertilizer (Decrease FertilizerUse)
  //Phase out the subsidy of fertilizer (Decrease Economy)
  //low use of inorganic fertilizer (Decrease FertilizerUse)
  //less water (Decrease Water)
  //reduced farm sizes (Decrease HouseholdSize)

  "sent4" should "have 5 decrease events" in {
    eventsDecrease4 should have size(5)
  }

  "sent4" should "have 2 FertilizerUse decrease events" in {
    val filteredEvents = eventsDecrease4.filter(_.arguments("theme").head.label == "FertilizerUse")
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("use of inorganic fertilizer")
    //missing "put down inorganic fertilizer"
    filteredEvents should have size (2)
  }

  "sent4" should "have 1 Subsidy decrease event" in {
    val filteredEvents = eventsDecrease4.filter(_.arguments("theme").head.label == "Subsidy")
    filteredEvents should have size (1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("the fertilizer subsidy")
  }

  "sent4" should "have 1 Water decrease event" in {
    val filteredEvents = eventsDecrease4.filter(_.arguments("theme").head.label == "Water")
    filteredEvents should have size (1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("water")
  }

  "sent4" should "have 1 HouseholdSize decrease event" in {
    val filteredEvents = eventsDecrease4.filter(_.arguments("theme").head.label == "HouseholdSize")
    filteredEvents should have size (1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("farm sizes")
  }

  // CAUSE-EFFECT EVENTS:
  //policy to cut use of inorganic fertilizer ... results in deteriorating biophysical conditions
  //phase out the fertilizer subsidy results in deteriorating biophysical conditions
  //low use of inorganic fertilizer .. lead to low benefit from the improved cultivar
  //less water ... lead to low benefit from the improved cultivar
  //reduced farm sizes which lead to low benefit from the improved cultivar

  "sent4" should "have 5 cause-effect events" in {
    eventsCause4 should have size(5)
  }

  "sent4" should "extract CAUSE event from: 'policy to cut use of inorganic fertilizer ... results in deteriorating" +
      "biophysical conditions'" in {
    val cause1 = eventsCause4.filter(e =>
          (e.arguments("cause").head.label == DECREASE) &&
          (e.arguments("cause").head.text == "cut use of inorganic fertilizer")
    )
    cause1 should have size (1) // there should be 1 cause-effect event with cause == DECREASE  with above text
    val effects1 = cause1.head.arguments("effect")
    effects1 should have size (1)
    effects1.head.text should be ("deteriorating biophysical conditions")
  }

  "sent4" should "extract CAUSE event from: 'phase out the fertilizer subsidy results in deteriorating biophysical conditions'" in {
    val cause1 = eventsCause4.filter(e =>
      (e.arguments("cause").head.label == DECREASE) &&
          (e.arguments("cause").head.text == "phase out the fertilizer subsidy")
    )
    cause1 should have size (1) // there should be 1 cause-effect event with cause == DECREASE  with above text
    val effects1 = cause1.head.arguments("effect")
    effects1 should have size (1)
    effects1.head.text should be ("deteriorating biophysical conditions")
  }

  "sent4" should "extract CAUSE event from: 'low use of inorganic fertilizer ... lead to low benefit from the improved cultivar'" in {
    val cause1 = eventsCause4.filter(e =>
      (e.arguments("cause").head.label == DECREASE) &&
          (e.arguments("cause").head.text == "low use of inorganic fertilizer")
    )
    cause1 should have size (1) // there should be 1 cause-effect event with cause == DECREASE  with above text
    val effects1 = cause1.head.arguments("effect")
    effects1 should have size (1)
    effects1.head.text should be ("low benefit from the improved cultivar")
  }

  "sent4" should "extract CAUSE event from: 'less water ... lead to low benefit from the improved cultivar'" in {
    val cause1 = eventsCause4.filter(e =>
      (e.arguments("cause").head.label == DECREASE) &&
          (e.arguments("cause").head.text == "less water")
    )
    cause1 should have size (1) // there should be 1 cause-effect event with cause == DECREASE  with above text
    val effects1 = cause1.head.arguments("effect")
    effects1 should have size (1)
    effects1.head.text should be ("low benefit from the improved cultivar")
  }

  "sent4" should "extract CAUSE event from: 'reduced farm sizes ... lead to low benefit from the improved cultivar'" in {
    val cause1 = eventsCause4.filter(e =>
      (e.arguments("cause").head.label == DECREASE) &&
          (e.arguments("cause").head.text == "reduced farm sizes")
    )
    cause1 should have size (1) // there should be 1 cause-effect event with cause == DECREASE  with above text
    val effects1 = cause1.head.arguments("effect")
    effects1 should have size (1)
    effects1.head.text should be ("low benefit from the improved cultivar")
  }


  val sent5 = "With increases in poverty levels people become more vulnerable to climate change and other risks."

  "sent5" should "have one IncreaseEvent" in {
    val mentions = extractMentions(sent5)
    val events = mentions.filter(_ matches INCREASE) //
    events should have size (1)

    for (e <- events) {
      e.arguments.get("theme").get should have size (1)
      e.arguments.get("theme").get.exists(_.label == "Poverty")
      e.arguments.get("theme").get.exists(_.text == "poverty levels") should be(true)
    }
  }


  val sent6 = "There will be a small increase in crop diversity due to the need to combat the climate and market risks as both of these might become more volatile in the future."
  //small increase in crop diversity (Increase in Crop)
  //increase in crop due to "the need to combat the climate and market risks (Increase due to NounPhrase)

  val mentions6 = extractMentions(sent6)

  "sent6" should "have 1 Increase Event for Crop with a Quantifier" in {
    val events = mentions6.filter(_ matches INCREASE) //
    events should have size (1)

    for (e <- events) {
      e.arguments.get("theme").get should have size (1)
      e.arguments.get("theme").get.exists(_.label == "Crop")
      e.arguments.get("theme").get.exists(_.text == "in crop diversity") should be(true) //will pass with "in" but probably shouldn't have it here
      e.arguments.get("quantifier").get should have size (1)
      e.arguments.get("quantifier").get.exists(_.label == "Quantifier")
      e.arguments.get("quantifier").get.exists(_.text == "small") should be(true)
    }
  }

  "sent6" should "have 1 Cause-and-Effect Event" in {
    val events = mentions6.filter(_ matches CAUSEEF) //
    events should have size (1)

    for (e <- events) {
      e.arguments.keySet should contain("cause")
      e.arguments.get("cause").get should have size (1)
      e.arguments.get("cause").get.exists(_.label == "NounPhrase")
      e.arguments.get("cause").get.exists(_.text == "the need to combat the climate and market risks") should be(true)

      e.arguments.keySet should contain("effect")
      e.arguments.get("effect").get should have size (1)
      e.arguments.get("effect").get.exists(_.label == "Increase")
      e.arguments.get("effect").get.exists(_.text == "small increase in crop diversity") should be(true)
    }

  }


  val sent7 = "Significant decline in poverty will be associated with a decrease in family size and increase in non-farm income."
  val mentions7 = extractMentions(sent7)
  //Increase in non-farm income (Increase Economy)
  //2 Decrease Events: decline in Poverty & decrease in family size (Population)

  "sent7" should "have 1 Increase Event for Economy" in {
    val events = mentions7.filter(_ matches INCREASE) //
    events should have size (1)

    for (e <- events) {
      e.arguments.get("theme").get should have size (1)
      e.arguments.get("theme").get.exists(_.label == "Economy")
      e.arguments.get("theme").get.exists(_.text == "non-farm income") should be(true)

    }
  }

  "sent7" should "have 1 Decrease Event for Poverty" in {
    val events = mentions7.filter(_ matches DECREASE) //
    val filteredEvents = events.filter(_.arguments("theme").head.label == "Poverty")
    filteredEvents should have size (1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("poverty")
    val filteredQuants = events.filter(_.arguments("quantifier").head.label == "Quantifier")
    filteredQuants should have size (1)
    val quants = filteredQuants.map(_.arguments("quantifier").head.text)
    quants should contain("Significant")

  }

  //
  "sent7" should "have 1 Decrease Event for Population" in {
    val events = mentions7.filter(_ matches DECREASE) //
    val filteredEvents = events.filter(_.arguments("theme").head.label == "HouseholdSize")
    filteredEvents should have size (1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("family size")
  }


    //Increase in Poverty
  val sent8 = "Poverty levels continue to increase, people become more vulnerable to food insecurity and other risks."
  val mentions8 = extractMentions(sent8)

  "sent8" should "have 1 Increase Event for Population" in {
    val events = mentions8.filter(_ matches INCREASE)
    events should have size(1)
    val filteredEvents = events.filter(_.arguments("theme").head.label == "Poverty")
    filteredEvents should have size (1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("Poverty levels")
  }


  //Increase water irrigation/management system
  //DUE TO? Increase Water due to Drought?
  val sent9 = "Government puts more emphasis on improving the agricultural water irrigation/management system to cope with drought conditions."
  val mentions9 = extractMentions(sent9)

  "sent9" should "have 1 Increase Event for Water" in {
    val events = mentions9.filter(_ matches INCREASE)
    events should have size(1)
    val filteredEvents = events.filter(_.arguments("theme").head.label == "Water")
    filteredEvents should have size (1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("the agricultural water irrigation/management system")

  }

  //Increase in Subsidy
  val sent10 = "Main interventions will include support for the agricultural-service sector, fertilizer subsidies, and feeder roads (slow)."

  "sent10" should "have 1 Increase Event for Subsidy" in {
    val mentions = extractMentions(sent10)
    val events = mentions.filter(_ matches INCREASE)
    events should have size(1)
    val filteredEvents = events.filter(_.arguments("theme").head.label == "Subsidy")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("fertilizer subsidies")

  }

  //2 increase events (Crops & FertilizerUse), 1 decrease event (FertilizerPrice)
  val sent11 = "The government promotes high-yielding and drought-/flood-tolerant rice varieties with policy to encourage the application of organic fertilizers, decreasing the cost on inorganic fertilizers."
  val mentions11 = extractMentions(sent11)
  val eventsIncrease11 = mentions11.filter(_ matches INCREASE)
  val eventsDecrease11 = mentions11.filter(_ matches DECREASE)

  "sent11" should "have 2 Increase Events overall" in {
    eventsIncrease11 should have size(2)
  }

  "sent11" should "have 1 Increase Event for Crop (rice)" in {
    val filteredEvents = eventsIncrease11.filter(_.arguments("theme").head.label == "Crop")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("rice varieties") // ??? i don't know ???
  }

  "sent11" should "have 1 Increase Event for FertilizerUse" in {
    val filteredEvents = eventsIncrease11.filter(_.arguments("theme").head.label == "FertilizerUse")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("the application of organic fertilizers")
  }

  "sent11" should "have 1 Decrease Event for FertilizerPrice" in {
    eventsDecrease11 should have size(1)
    val filteredEvents = eventsDecrease11.filter(_.arguments("theme").head.label == "FertilizerPrice")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("the cost on inorganic fertilizers")

  }


  val sent12 = "Use of improved cultivars and mechanization will be increased and use of critical interventions may lead to increases in productivity and efficient use of resources."

  "sent12" should "have 1 Increase Event for Productivity" in {
    val mentions = extractMentions(sent12)
    val events = mentions.filter(_ matches INCREASE) //Vector[Mentions]
    events should have size(1)
    val filteredEvents = events.filter(_.arguments("theme").head.label == "Productivity")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("productivity")

  }

  //Params: Increased Investment and Increase Population
  val sent13 = "Therefore, the government is committed to supporting the agriculture sector through increased public investment to fulfill the needs of an increasing population."
  val mentions13 = extractMentions(sent13)
  val eventsIncrease13 = mentions13.filter(_ matches INCREASE)

  "sent13" should "have 2 Increase Events" in {
    eventsIncrease13 should have size(2)
  }

  "sent13" should "have 1 IncreaseEvent for Subsidy" in {
    val filteredEvents = eventsIncrease13.filter(_.arguments("theme").head.label == "Subsidy")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("public investment")
  }

  "sent13" should "have 1 IncreaseEvent for Population" in {
    val filteredEvents = eventsIncrease13.filter(_.arguments("theme").head.label == "HouseholdSize")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("population")
  }


  val sent14 = "Water quality and water availability for agriculture will decrease due to pollution of water bodies, " +
  "and competition for water from other sources, " +
    "but water-use efficiency in agriculture will increase due to technological progress."

  val mentions14 = extractMentions(sent14)
  val eventsDecrease14 = mentions14.filter(_ matches DECREASE)
  val eventsIncrease14 = mentions14.filter(_ matches INCREASE)
  val eventsCause14 = mentions14.filter(_ matches CAUSEEF)

  //Water Increase 1x, Technology Increase 1x
  "sent14" should "have 2 Increase Events" in {
    eventsIncrease14 should have size(2)
  }

  "sent14" should "have 1 Increase Event for Water" in {
    val filteredEvents = eventsIncrease14.filter(_.arguments("theme").head.label == "Water")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("water-use efficiency")
  }

  "sent14" should "have 1 Increase Event for Productivity" in {
    val filteredEvents = eventsIncrease14.filter(_.arguments("theme").head.label == "Productivity")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("technological")
  }

  //Water Decrease 2x
  "sent14" should "have 2 Decrease Events for Water" in {
    val filteredEvents = eventsIncrease14.filter(_.arguments("theme").head.label == "Water")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("Water quality")
    themes should contain("water availability for agriculture") //fails here!

    eventsDecrease14 should have size(2)
  }

  //C&E:
  // Water quality decrease due to pollution
  // water availability decrease due to pollution
  // water-use efficiency Increase due to tech progress
  "sent14" should "have 3 Cause-and-Effect Events" in {
    eventsCause14 should have size(3)
  }

  "sent14" should "have 2 Cause-and-Effect Events for Decrease in Water due to Pollution" in {
    val filteredEffects = eventsCause14.filter(_.arguments("effect").head.label == "Decrease")
    filteredEffects should have size(2)
    val effects = filteredEffects.map(_.arguments("theme").head.text)
    effects should contain("Water quality and water availability for agriculture will decrease")
    val filteredCauses = eventsCause14.filter(_.arguments("cause").head.label == "Pollution")
    filteredCauses should have size(2) //technically the same thing twice? so is it size 1?
    filteredCauses should contain("pollution of water bodies")
  }

  //but water-use efficiency in agriculture will increase due to technological progress
  "sent14" should "have 1 Cause-and-Effect Event for Increase Water due to Increase Productivity " in {
    val filteredEffects = eventsCause14.filter(_.arguments("effect").head.label == "Increase")
    filteredEffects should have size(2)
    val effects = filteredEffects.map(_.arguments("theme").head.text)
    effects should contain("water-use efficiency in agriculture will increase")
    val filteredCauses = eventsCause14.filter(_.arguments("cause").head.label == "Increase")
    filteredCauses should have size(1)
    filteredCauses should contain("technological progress")
  }


  val sent15 = "Farm size and wage rates will increase."
  val mentions15 = extractMentions(sent15)
  val eventsIncrease15 = mentions15.filter(_ matches INCREASE)


  "sent15" should "have 2 Increase Events" in {
    eventsIncrease15 should have size(2)
  }

  "sent15" should "have 1 Increase Event for FarmSize" in {
    val filteredEvents = eventsIncrease14.filter(_.arguments("theme").head.label == "FarmSize")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("Farm size")

  }

  "sent15" should "have 1 Increase Event for Economy" in {
    val filteredEvents = eventsIncrease14.filter(_.arguments("theme").head.label == "Economy")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("wage rates")

  }


  val sent16 = "Mechanization and energy-use intensity in agriculture will increase."

  "sent16" should "have 2 Increase Events for Productivity" in {
    val mentions = extractMentions(sent16)
    val eventsIncrease = mentions.filter(_ matches INCREASE)
    val filteredEvents = eventsIncrease.filter(_.arguments("theme").head.label == "Productivity")
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("Mechanization")
    themes should contain("energy-use intensity in agriculture")
    eventsIncrease should have size(2)
  }




  val sent17 = "Fertilizer-use intensity and fertilizer productivity will increase."
  val mentions17 = extractMentions(sent17)
  val eventsIncrease17 = mentions17.filter(_ matches INCREASE)

  "sent17" should "have 2 Increase Events" in {
    eventsIncrease17 should have size(2)
  }

  "sent17" should "have 1 Increase Event for FertilizerUse" in {
    val filteredEvents = eventsIncrease17.filter(_.arguments("theme").head.label == "FertilizerUse")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("Fertilizer-use intensity")
  }

  "sent17" should "have 1 Increase Event for Productivity" in {
    val filteredEvents = eventsIncrease17.filter(_.arguments("theme").head.label == "Productivity")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("fertilizer productivity")
  }

  //2 Increase, 1 CE
  val sent18 = "There will not be significant changes in food imports, while yield of important crops will increase due to technological progress in agriculture."
  val mentions18 = extractMentions(sent18)
  val eventsIncrease18 = mentions18.filter(_ matches INCREASE)

  "sent18" should "have 2 Increase Events" in {
    eventsIncrease18 should have size(2)
  }

  "sent18" should "have 1 Increase Event for Crop" in {
    val filteredEvents = eventsIncrease18.filter(_.arguments("theme").head.label == "Crop")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("yield of important crops")
  }

  "sent18" should "have 1 Increase Event for Productivity" in {
    val filteredEvents = eventsIncrease18.filter(_.arguments("theme").head.label == "Productivity")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("technological")
  }

  "sent18" should "have 1 Cause-and-Effect Event" in  {
    val eventsCause = mentions18.filter(_ matches CAUSEEF)
    eventsCause should have size(1)

    val filteredEffects = eventsCause.filter(_.arguments("effect").head.label == "Increase")
    filteredEffects should have size(2)
    val effects = filteredEffects.map(_.arguments("theme").head.text)
    effects should contain("yield of important crops will increase")

    val filteredCauses = eventsCause.filter(_.arguments("cause").head.label == "NounPhrase") //should be another Increase?
    filteredCauses should have size(1)
    filteredCauses should contain("technological progress in agriculture")
  }

  val sent19 = "This requires more opportunities in non-agricultural income and increased technological interventions."
  val mentions19 = extractMentions(sent19)
  val eventsIncrease19 = mentions19.filter(_ matches INCREASE)

  "sent19" should "have 2 Increase Events" in {
    eventsIncrease19 should have size(2)
  }

  "sent19" should "have 1 Increase Event for Economy" in {
    val filteredEvents = eventsIncrease19.filter(_.arguments("theme").head.label == "Economy")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("opportunities in non-agricultural income")
  }

  "sent19" should "have 1 Increase Event for Productivity" in {
    val filteredEvents = eventsIncrease19.filter(_.arguments("theme").head.label == "Productivity")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("technological interventions")
  }

  val sent20 = "However, opportunities for massive increases in agricultural production and productivity exist but are not being exploited."

  "sent20" should "have 2 Increase Events for Productivity" in {
    val mentions = extractMentions(sent20)
    val eventsIncrease = mentions.filter(_ matches INCREASE)

    val filteredEvents = eventsIncrease.filter(_.arguments("theme").head.label == "Productivity")

    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("productivity")
    themes should contain("agricultural production")

    val filteredQuants = eventsIncrease.filter(_.arguments("quantifier").head.label == "Quantifier")
    val quants = filteredQuants.map(_.arguments("quantifier").head.text)
    quants should contain("massive")

    filteredEvents should have size(2)
    filteredQuants should have size(2)
    eventsIncrease should have size(2)

  }

  val sent21 = "The governmental policy objective is to achieve food security, ensure adequate raw materials for the manufacturing sector, and increased export earnings through increased productivity, efficient input use, and better market access, infrastructure, and service development."
  val mentions21 = extractMentions(sent21)
  val eventsIncrease21 = mentions21.filter(_ matches INCREASE)

  "sent21" should "have 3 Increase Events" in {
    eventsIncrease21 should have size(2)
  }

  "sent21" should "have 1 Increase Event for Economy" in {
    val filteredEvents = eventsIncrease21.filter(_.arguments("theme").head.label == "Economy")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("market access")
  }

  "sent21" should "have 1 Increase Event for Productivity" in {
    val filteredEvents = eventsIncrease21.filter(_.arguments("theme").head.label == "Productivity")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("productivity")
  }


  val sent22 = "Hence, government liberalizes imports of food grains, invests in food chain logistics, and boost research and development for new crop cultivars to boost agricultural production for ensuring food security."
  val mentions22 = extractMentions(sent22)
  val eventsIncrease22 = mentions22.filter(_ matches INCREASE)

  "sent22" should "have 3 Increase Events" in {
    eventsIncrease22 should have size(3)
  }

  "sent22" should "have 2 Increase Events for Productivity" in {
    val filteredEvents = eventsIncrease22.filter(_.arguments("theme").head.label == "Productivity")
    filteredEvents should have size(2)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("food chain logistics")
    themes should contain("agricultural production")
  }

  "sent22" should "have 1 Increase Events for Crop" in {
    val filteredEvents = eventsIncrease22.filter(_.arguments("theme").head.label == "Crop")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("new crop cultivars")
  }


  //4 increase
  val sent23 = "A combination of increasing population, government plans to invest in fertilizer factory, government subsidy on fertilizers, improved economic performance expected to cause a shift from agriculture to service industry, government plans for massive expansion of irrigation (irrigate 1 million ha.), newly devolved county governments etc. are some of the developments expected to change agriculture development in the country."
  val mentions23 = extractMentions(sent23)
  val eventsIncrease23 = mentions23.filter(_ matches INCREASE)

  "sent23" should "have 4 Increase Events" in {
    eventsIncrease23 should have size(4)
  }

  "sent23" should "have 1 Increase Event for HouseholdSize" in {
    val filteredEvents = eventsIncrease23.filter(_.arguments("theme").head.label == "HouseholdSize")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("population")
  }

  "sent23" should "have 1 Increase Event for FertilizerUse" in {
    val filteredEvents = eventsIncrease23.filter(_.arguments("theme").head.label == "FertilizerUse")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("fertilizer factory")
  }

  "sent23" should "have 1 Increase Event for Water" in {
    val filteredEvents = eventsIncrease23.filter(_.arguments("theme").head.label == "Water")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("irrigation")

    val filteredQuants = eventsIncrease23.filter(_.arguments("quantifier").head.label == "Quantifier")
    filteredQuants should have size(1)
    val quants = filteredQuants.map(_.arguments("quantifier").head.text)
    quants should contain("massive")
  }

  "sent23" should "have 1 Increase Event for Economy" in {
    val filteredEvents = eventsIncrease23.filter(_.arguments("theme").head.label == "Economy")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("economic performance")
  }


  //increase subsidies (Subsidies) x2
  //extending irrigation services (Water) x1
  //Missing: extending ...  mechanization (Productivity) x1
  val sent24 = "Along with the support programs such as agricultural insurance and input subsidies, the government efforts and investments will be increased for extending irrigation services, agricultural mechanization, and developing disaster risk-management practices."
  val mentions24 = extractMentions(sent24)
  val eventsIncrease24 = mentions24.filter(_ matches INCREASE)

  "sent24" should "have 4 Increase Events" in {
    eventsIncrease24 should have size(4)
  }

  "sent24" should "have 2 Increase Events for Subsidy" in {
    val filteredEvents = eventsIncrease24.filter(_.arguments("theme").head.label == "Subsidy")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("investments")
    themes should contain("input subsidies")
  }

  "sent24" should "have 1 Increase Events for Water" in {
    val filteredEvents = eventsIncrease24.filter(_.arguments("theme").head.label == "Water")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("irrigation services")
  }

  "sent24" should "have 1 Increase Events for Productivity" in {
    val filteredEvents = eventsIncrease24.filter(_.arguments("theme").head.label == "Productivity")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("agricultural mechanization")
  }

  val sent25 = "The transformation however starts under extremely difficult conditions, characterized by large account deficit and liquidity challenges and limited direct foreign investment due to lack of clarity on investment security and high interest rates."
  val mentions25 = extractMentions(sent25)

  "sent25" should "have 1 Decrease Event for Subsidy" in {
    val events = mentions25.filter(_ matches DECREASE)
    events should have size(1)
    val filteredEvents = events.filter(_.arguments("theme").head.label == "Subsidy")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("direct foreign investment")
  }

  "sent25" should "have 1 Cause and Effect Event" in {
    val events = mentions25.filter(_ matches CAUSEEF)
    events should have size(1)
    val filteredCauses = events.filter(_.arguments("cause").head.label == "NounPhrase")
    filteredCauses should have size(1)
    val causes = filteredCauses.map(_.arguments("cause").head.text)
    causes should contain("lack of clarity on investment security")

    val filteredEffects = events.filter(_.arguments("effect").head.label == "Decrease")
    filteredEffects should have size(1)
    val effects = filteredEffects.map(_.arguments("effect").head.text)
    effects should contain("limited direct foreign investment")
  }

  val sent26 = "Global trends suggest that rice–wheat production in the region will be adversely affected by climate change."
  val mentions26 = extractMentions(sent26)

  "sent26" should "have 1 Decrease Event for Crop" in {
    val events = mentions26.filter(_ matches DECREASE)
    events should have size(1)
    val filteredEvents = events.filter(_.arguments("theme").head.label == "Decrease")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("rice")
  }

  "sent26" should "have 1 Cause and Effect Event" in {
    val events = mentions26.filter(_ matches CAUSEEF)
    events should have size(1)
    val filteredCauses = events.filter(_.arguments("cause").head.label == "Climate")
    filteredCauses should have size(1)
    val causes = filteredCauses.map(_.arguments("cause").head.text)
    causes should contain("climate change")

    val filteredEffects = events.filter(_.arguments("effect").head.label == "Decrease")
    filteredEffects should have size(1)
    val effects = filteredEffects.map(_.arguments("effect").head.text)
    effects should contain("rice–wheat production in the region will be adversely affected")
  }


  //TODO: is "prices of ag commodities will increase" an Increase Event for Crop?
  val sent27 = "Most subsidies are likely to decline while prices of agricultural commodities will increase."
  val mentions27 = extractMentions(sent27)

  "sent27" should "have 1 Decrease Event for Subsidy" in {
    val events = mentions27.filter(_ matches DECREASE)
    events should have size(1)
    val filteredEvents = events.filter(_.arguments("theme").head.label == "Subsidy")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("subsidies")
  }


  val sent28 = "With a high cost of production and degraded natural resources, profitability in agriculture may be further reduced, making agriculture unprofitable."
  val mentions28 = extractMentions(sent28)

  "sent28" should "have 1 Decrease Event for Economy" in {
    val events = mentions28.filter(_ matches DECREASE)
    events should have size(1)
    val filteredEvents = events.filter(_.arguments("theme").head.label == "Economy")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("profitability in agriculture")

  }

  "sent28" should "NOT have 1 Increase Event" in {
    val events = mentions28.filter(_ matches INCREASE)
    events should have size(0)
  }


  val sent29 = "Labor migration to urban areas, non-agricultural activities and impact of HIV/AIDS also leads to labor shortages."

  "sent29" should "have 1 Decrease Event for Labor" in {
    val mentions = extractMentions(sent29)
    val events = mentions.filter(_ matches DECREASE)
    events should have size(1)

    val filteredEvents = events.filter(_.arguments("theme").head.label == "Labor")
    filteredEvents should have size(1)

    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("labor")

  }

  //TODO: should "profitability" be Economy Decrease?
  val sent30 = "Agricultural production and profitability are declining, land is degrading and being underutilized."

  "sent30" should "have 1 Decrease Event for Productivity" in {
    val mentions = extractMentions(sent30)
    val events = mentions.filter(_ matches DECREASE)
    events should have size(1)

    val filteredEvents = events.filter(_.arguments("theme").head.label == "Productivity")
    filteredEvents should have size(1)

    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("Agricultural production")

  }

  //2 Decrease and 2 Cause and Effect
  val sent31 = "Soil quality will decline by a small-to-medium extent, due to pollution, and intensive cultivation will be caused by a shrinking land base for agriculture."
  val mentions31 = extractMentions(sent31)
  val eventsDecrease31 = mentions31.filter(_ matches DECREASE)
  val eventsCause31 = mentions31.filter(_ matches CAUSEEF)

  "sent31" should "have 2 Decrease Events" in {
    eventsDecrease31 should have size(2)
  }

  "sent31" should "have 1 Decrease Event for FarmSize" in {
    val filteredEvents = eventsDecrease31.filter(_.arguments("theme").head.label == "FarmSize")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("land base for agriculture")
  }

  "sent31" should "have 1 Decrease Event for Soil" in {
    val filteredEvents = eventsDecrease31.filter(_.arguments("theme").head.label == "Soil")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("Soil quality")

    val filteredQuants = eventsDecrease31.filter(_.arguments("quantifier").head.label == "Quantifier")
    val quants = filteredQuants.map(_.arguments("theme").head.text)
    quants should contain("small-to-medium")
  }

  "sent31" should "have 2 Cause and Effect Events" in {
    eventsCause31 should have size(2)
  }

  "sent31" should "have 1 Cause and Effect Event for Decrease in FarmSize" in {
    val filteredCauses = eventsCause31.filter(_.arguments("cause").head.label == "Decrease")
    filteredCauses should have size(1)
    val causes = filteredCauses.map(_.arguments("cause").head.text)
    causes should contain("shrinking land base for agriculture")

    val filteredEffects = eventsCause31.filter(_.arguments("effect").head.label == "NounPhrase")
    filteredEffects should have size(1)
    val effects = filteredEffects.map(_.arguments("effect").head.text)
    effects should contain("intensive cultivation")
  }

  "sent31" should "have 1 Cause and Effect Event for Decrease in Soil" in {
    val filteredCauses = eventsCause31.filter(_.arguments("cause").head.label == "Pollution")
    filteredCauses should have size(1)
    val causes = filteredCauses.map(_.arguments("cause").head.text)
    causes should contain("pollution")

    val filteredEffects = eventsCause31.filter(_.arguments("effect").head.label == "Decrease")
    filteredEffects should have size(1)
    val effects = filteredEffects.map(_.arguments("effect").head.text)
    effects should contain("Soil quality will decline by a small-to-medium extent")
  }


  //should have a CauseEf "with the consequence of"
  val sent32 = "The government aims to invest more in agriculture, shortage of labor with the consequence of decreased population growth and household size."
  val mentions32 = extractMentions(sent23)
  val eventsDecrease32 = mentions32.filter(_ matches DECREASE)
  //decrease HouseholdSize x2 //labor shortage

  "sent32" should "have 3 Decrease Events" in {
    eventsDecrease32 should have size(3)
  }

  "sent32" should "have 2 Decrease Events for HouseholdSize" in {
    val filteredEvents = eventsDecrease32.filter(_.arguments("theme").head.label == "HouseholdSize")
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("population growth")
    themes should contain("household size")

    filteredEvents should have size(2)
    themes should have size(2)

  }

  "sent32" should "have 1 Decrease Event for Labor" in {
    val filteredEvents = eventsDecrease32.filter(_.arguments("theme").head.label == "Labor")
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("labor")
    //currently contains "of labor with the consequence of"

    filteredEvents should have size(1)
    themes should have size(1)
  }

  "sent32" should "have 2 Cause and Effect Events for the 2 Decrease Household Events" in {
    val events = mentions32.filter(_ matches CAUSEEF)

    //cause is the shortage of labor
    val filteredCause = events.filter(_.arguments("cause").head.label == "Decrease")
    filteredCause should have size(1)
    val cause = filteredCause.map(_.arguments("cause").head.text)
    cause should contain("shortage of labor")

    val filteredEffects = events.filter(_.arguments("effect").head.label == "Decrease")
    filteredEffects should have size(2)
    val effects = filteredEffects.map(_.arguments("cause").head.text)
    effects should contain("decrease population growth")
    effects should contain("household size")
  }



  val sent34 = "Climate change remains as a key challenge for a country like Nepal where subsistence-based and rainfed agriculture system is dominant."

  "sent34" should "have 1 Remain Event for Climate" in {
    val mentions = extractMentions(sent34)
    val events = mentions.filter(_ matches REMAIN)
    events should have size(1)
    val filteredEvents = events.filter(_.arguments("theme").head.label == "Climate")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("Climate change")
  }

  //TODO: This has a cause and effect, but it doesn't contain Param Increase/Decreases. Do we still want to capture it?
  val sent35 = "Most people remain in rural areas where agriculture is the main livelihood activity due to lack of alternatives."
  val mentions35 = extractMentions(sent35)

  "sent35" should "have 1 Remain Event for HouseholdSize" in {
    val events = mentions35.filter(_ matches REMAIN)
    events should have size(1)
    val filteredEvents = events.filter(_.arguments("theme").head.label == "HouseholdSize")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("people")
  }

  val sent36 = "Labor migration and HIV/AIDS result in labor shortage."
  val mentions36 = extractMentions(sent36)

  "sent36" should "have 1 Decrease Event for Labor" in {
    val events = mentions36.filter(_ matches DECREASE)
    events should have size(1)
    val filteredEvents = events.filter(_.arguments("theme").head.label == "Labor")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("labor")
  }

  "sent36" should "have 1 Cause and Effect Event for Decrease of Labor" in {
    val events = mentions36.filter(_ matches CAUSEEF)
    events should have size(1)

    val filteredCauses = events.filter(_.arguments("cause").head.label == "NounPhrase")
    filteredCauses should have size(1)
    val cause = filteredCauses.map(_.arguments("cause").head.text)
    cause should contain("HIV/AIDS")

    val filteredEffects = events.filter(_.arguments("effect").head.label == "Decrease")
    filteredEffects should have size(1)
    val effect = filteredEffects.map(_.arguments("effect").head.text)
    effect should contain("labor shortage")

  }


  //cause: increase in inequality
  //effect: economy decreases
  val sent37 = "Share of agriculture in overall economy will decrease with increase in inequality."
  val mentions37 = extractMentions(sent37)

  "sent37" should "have 1 Decrease Event for Economy" in {
    val events = mentions37.filter(_ matches DECREASE)
    events should have size(1)

    val filteredEvents = events.filter(_.arguments("theme").head.label == "Economy")
    filteredEvents should have size(1)

    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("Share of agriculture in overall economy")
  }

  "sent37" should "have 1 Cause and Effect Event" in {
    val events = mentions37.filter(_ matches CAUSEEF)
    events should have size(1)

    val filteredCauses = events.filter(_.arguments("cause").head.label == "VerbPhrase")
    filteredCauses should have size(1)
    val cause = filteredCauses.map(_.arguments("cause").head.text)
    cause should contain("increase in inequality")

    val filteredEffects = events.filter(_.arguments("effect").head.label == "Decrease")
    filteredEffects should have size(1)
    val effect = filteredEffects.map(_.arguments("effect").head.text)
    effect should contain("Share of agriculture in overall economy will decrease")
  }

  //cause: anticipated losses
  //effect: adoption process instigated
  val sent38 = "The adoption process will be instigated due to the anticipated losses in agricultural productivity in the face of climatic uncertainties."
  val mentions38 = extractMentions(sent38)

  "sent38" should "have 1 Decrease Event for Productivity" in {
    val events = mentions38.filter(_ matches DECREASE)
    events should have size(1)
    val filteredEvents = events.filter(_.arguments("theme").head.label == "Productivity")
    filteredEvents should have size(1)
    val themes = filteredEvents.map(_.arguments("theme").head.text)
    themes should contain("agricultural productivity")
  }

  "sent38" should "have 1 Cause and Effect Event" in {
    val events = mentions38.filter(_ matches CAUSEEF)
    events should have size(1)

    val filteredCauses = events.filter(_.arguments("cause").head.label == "Decrease")
    filteredCauses should have size(1)
    val cause = filteredCauses.map(_.arguments("cause").head.text)
    cause should contain("the anticipated losses in agricultural productivity in the face of climatic uncertainties")

    val filteredEffects = events.filter(_.arguments("effect").head.label == "NounPhrase")
    filteredEffects should have size(1)
    val effect = filteredEffects.map(_.arguments("effect").head.text)
    effect should contain("The addoption process")
  }

  //TODO: SENTENCES I"M UNSURE ABOUT
  //increase44 "prioritize its programs" an increaseEvent ????
  val increase44 = "However, the government will prioritize its programs to minimize the loss from climate change impacts and reduce the vulnerability of the people."

  //Not sure if "high cost of production" should be counted as an IncreaseEvent?? increase9 & 10
  val increase9a = "With the high cost of production, food imports will further reduce farmers’ chances to make a living from agriculture."
  val increase9b = "With a high cost of production and degraded natural resources, profitability in agriculture may be further reduced, making agriculture unprofitable."
  val increase10 = "The transformation however starts under extremely difficult conditions, characterized by large account deficit and liquidity challenges and limited direct foreign investment due to lack of clarity on investment security and high interest rates."

  //decrease in imports??
  val decrease11 = "Imports are inadequate to meet domestic demand."
  val decrease12 = "Incentives in the form of assured prices (minimum support prices) are inadequate to enhance agricultural production to meet food demand."

  //cause: public sector & private sector OR just the "limit development" ??
  //effect: unsustainable import bills
  //TODO: I'm unsure if this sentence should be included? What params are decreasing?
  val ce = "Underfunded public sector and underperformance of the private sector limit development of the agricultural sector and result in unsustainable import bills for agricultural commodities."


} //END OF TEST BRACE


