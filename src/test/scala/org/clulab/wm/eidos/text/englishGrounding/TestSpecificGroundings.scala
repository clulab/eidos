package org.clulab.wm.eidos.text.englishGrounding

import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigValueFactory
import org.clulab.wm.eidos.groundings.OntologyGrounding
import org.clulab.wm.eidos.groundings.PredicateGrounding
import org.clulab.wm.eidos.test.EidosTest
import org.clulab.wm.eidos.test.EnglishGroundingTest

// If these tests ever break, it is a very bad thing, even if some
// grounding change causes every other test in TestGrounding to pass.
class TestSpecificGroundings extends EnglishGroundingTest(TestSpecificGroundings.config) {

  case class Slots(concept: String, conceptProperty: String, process: String, processProperty: String)

  def getNameOrEmpty(ontologyGrounding: OntologyGrounding): String =
      ontologyGrounding.individualGroundings.headOption.map(_.name).getOrElse("")

  def getSlots(text: String, canonicalName: String): Slots = {
    val annotatedDocument = ieSystem.extractFromText(text)
    val eidosMention = annotatedDocument.allEidosMentions.find { eidosMention =>
      eidosMention.canonicalName == canonicalName
    }.get
    val grounding = eidosMention.grounding("wm_compositional").individualGroundings.head
    val predicateTuple = grounding.asInstanceOf[PredicateGrounding].predicateTuple

    val concept = getNameOrEmpty(predicateTuple.theme)
    val conceptProperty = getNameOrEmpty(predicateTuple.themeProperties)

    val process = getNameOrEmpty(predicateTuple.themeProcess)
    val processProperty = getNameOrEmpty(predicateTuple.themeProcessProperties)

    Slots(concept, conceptProperty, process, processProperty)
  }

  behavior of "compositional grounder"

  it should "ground 'water transportation' correctly" in {
    val text = "The price of oil decreased water transportation."
    val expectedSlots = Slots("wm/concept/goods/water", "", "wm/process/transportation/", "")
    val actualSlots = getSlots(text, "water transportation")

    actualSlots should be (expectedSlots)
  }

  it should "ground 'price of oil' correctly" in {
    val text = "The price of oil decreased water transportation."
    val expectedSlots = Slots("wm/concept/goods/fuel", "wm/property/price_or_cost", "", "")
    val actualSlots = getSlots(text, "oil")

    actualSlots should be (expectedSlots)
  }

  it should "ground 'food security' correctly" in {
    val text = "The present strategy emphasizes FAO's commitment to work with governments, animal health and production professionals, as well as farmers, to ultimately improve the livelihoods and food security of livestock owners and others along the value chain."
    val expectedSlots = Slots("wm/concept/goods/food/", "wm/property/security", "", "")
    val actualSlots = getSlots(text, "food security")

    actualSlots should be (expectedSlots)
  }

  it should "ground 'crop failure' correctly" in {
    val text = "Earlier\nthis year lack of rain led to crop failure and caused serious food shortages\nin many parts of the country, especially in the Afar region in the\nnorth-east."
    val expectedSlots = Slots("wm/concept/agriculture/crop/", "", "wm/process/failure", "")
    val actualSlots = getSlots(text, "crop failure")

    actualSlots should be (expectedSlots)
  }

  it should "ground 'food insecurity' correctly" in {
    val text = "Food insecurity can lead to migration pressure."
    val expectedSlots = Slots("wm/concept/goods/food/", "wm/property/insecurity", "", "")
    val actualSlots = getSlots(text, "Food insecurity")

    actualSlots should be (expectedSlots)
  }

  it should "ground 'crop production' correctly" in {
    val text = "Crop production promotes good nutrition."
    val expectedSlots = Slots("wm/concept/agriculture/crop/", "", "wm/process/production", "")
    val actualSlots = getSlots(text, "Crop")

    actualSlots should be (expectedSlots)
  }

  it should "ground 'nutrution' correctly" in {
    val text = "Successful crop production promotes good nutrition."
    val expectedSlots = Slots("wm/concept/health/nutrition", "", "", "")
    val actualSlots = getSlots(text, "nutrition")

    actualSlots should be (expectedSlots)
  }

  it should "ground 'food shortages' correctly" in {
    val text = "They also lack pasture and water for their livestock, all of which has lead to soaring food prices and food shortages."
    val expectedSlots = Slots("wm/concept/goods/food/", "wm/property/shortage", "", "")
    val actualSlots = getSlots(text, "food")

    actualSlots should be (expectedSlots)
  }
}

object TestSpecificGroundings {
  val config = ConfigFactory.load(EidosTest.groundingConfig)
      .withValue("ontologies.wm_compositional", ConfigValueFactory.fromAnyRef("/org/clulab/wm/eidos/english/ontologies/49277ea4-7182-46d2-ba4e-87800ee5a315.yml"))
}
