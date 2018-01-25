package org.clulab.wm

import org.scalatest._
import TestUtils._
import ReaderUtils._

class TestModifications extends FlatSpec with Matchers {


  //ONE Increase Event
  val sent1 = "Better and well-functioning agricultural credit and market services for both established and emerging farmers."

  sent1 should "have 1 Increase attachment for 'agricultural credit' " in {

    val mentions = extractMentions(sent1)
    val entities = mentions.filter(m => m.attachments.exists(a => a.isInstanceOf[Increase]))
    entities should have size (1)
    entities.head.attachments.head.asInstanceOf[Increase].trigger should be ("Better")
    entities.head.text should be ("well-functioning agricultural credit")

  }

  val sent2 = "The support for agricultural research, education, and extension programs will also be increased for " +
    "developing and disseminating climate change adaptation agricultural technologies to the farmers."
  // Note: parse of sentence makes it "impossible" to extract increase for education and extension programs --> maybe
  // a reason to switch to cluprocessor
  sent2 should "have 1 Increase attachment" in {
    val mentions = extractMentions(sent2)
    val entities = mentions.filter(m => m.attachments.exists(a => a.isInstanceOf[Increase]))
    entities should have size (1)
    entities.head.attachments.head.asInstanceOf[Increase].trigger should be ("increased")
    entities.head.text should be ("support for agricultural research")

  }


} //END OF TEST BRACE


