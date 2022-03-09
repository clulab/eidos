package org.clulab.wm.eidos.text.englishGrounding

import org.clulab.wm.eidos.groundings.OntologyGrounding
import org.clulab.wm.eidos.groundings.PredicateGrounding
import org.clulab.wm.eidos.test.EnglishGroundingTest

// If these tests ever break, it is a very bad thing, even if some
// grounding change causes every other test in TestGrounding to pass.
class TestSpecificGroundings extends EnglishGroundingTest {

  case class Slots(conceptOpt: Option[String], conceptPropertyOpt: Option[String], processOpt: Option[String], processPropertyOpt: Option[String])

  def getNameOpt(ontologyGrounding: OntologyGrounding): Option[String] =
      ontologyGrounding.individualGroundings.headOption.map(_.name)

  def getSlots(text: String, canonicalName: String): Slots = {
    val annotatedDocument = ieSystem.extractFromText(text)
    val foodSecurityMention = annotatedDocument.allEidosMentions.find { eidosMention =>
      eidosMention.canonicalName == canonicalName
    }.get
    val grounding = foodSecurityMention.grounding("wm_compositional").individualGroundings.head
    val predicateTuple = grounding.asInstanceOf[PredicateGrounding].predicateTuple

    val conceptOpt = getNameOpt(predicateTuple.theme)
    val conceptPropertyOpt = getNameOpt(predicateTuple.themeProperties)

    val processOpt = getNameOpt(predicateTuple.themeProcess)
    val processPropertyOpt = getNameOpt(predicateTuple.themeProcessProperties)

    Slots(conceptOpt, conceptPropertyOpt, processOpt, processPropertyOpt)
  }

  behavior of "grounder"

  it should "ground 'water transportation' correctly" in {
    val text = "The price of oil decreased water transportation."
    val expectedSlots = Slots(Some("wm/concept/goods/water"), None, Some("wm/process/transportation/"), None)
    val actualSlots = getSlots(text, "water transportation")

    actualSlots should be (expectedSlots)
  }

  it should "ground 'price of oil' correctly" in {
    val text = "The price of oil decreased water transportation."
    val expectedSlots = Slots(Some("wm/concept/goods/fuel"), Some("wm/property/price_or_cost"), None, None)
    val actualSlots = getSlots(text, "oil")

    actualSlots should be (expectedSlots)
  }

  it should "ground 'food security' correctly" in {
    val text = "The present strategy emphasizes FAO's commitment to work with governments, animal health and production professionals, as well as farmers, to ultimately improve the livelihoods and food security of livestock owners and others along the value chain."
    val expectedSlots = Slots(Some("wm/concept/goods/food/"), Some("wm/property/security"), None, None)
    val actualSlots = getSlots(text, "food security")

    actualSlots should be (expectedSlots)
  }
}
