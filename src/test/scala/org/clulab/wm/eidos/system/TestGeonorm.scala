package org.clulab.wm.eidos.system

import org.clulab.wm.eidos.attachments.Location
import org.clulab.wm.eidos.test.ExtractionTest

class TestGeonorm extends ExtractionTest {

  def locations(text: String): Seq[Location] = extractMentions(text).filter(_.label == "Location").flatMap(m => m.attachments.collect{
    case loc: Location => loc
  })

  it should "identify geographical locations" in {
    // The model sometimes generates spurious LOC tags in the padding needed for batch processing.
    // As of Jun 2019, this input produced some such tags, so check that no errors result.
    val text = """
      Fighting erupts in Juba and
      quickly spreads to Jonglei,
      Unity and Upper Nile.
      Parties to the conﬂict sign the Recommitment
      on Humanitarian Matters of the Cessation of
      Hostilities Agreement – including 30 days of
      tranquility – and subsequently the Agreement
      to Resolve the Crisis in South Sudan.
    """
    locations(text).map(_.text) should contain allElementsOf Set("Juba", "South Sudan")
    locations(text).flatMap(_.geoPhraseID.geonameID) should contain ("7909807") // South Sudan
    // (no test for Juba since it's ambiguous between the city and the state)

    // The model sometimes generates I-LOC tags at the beginning of a sentence.
    // As of Jun 2019, this input produced such a tag, so check that no errors result.
    val text2 = "b. Additional counties in need of some form of protection:"
    locations(text2) shouldBe empty
  }
}
