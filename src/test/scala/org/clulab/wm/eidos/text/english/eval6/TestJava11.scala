package org.clulab.wm.eidos.text.english.eval6

import org.clulab.processors.Document
import org.clulab.wm.eidos.graph.Correlation
import org.clulab.wm.eidos.graph.Dec
import org.clulab.wm.eidos.graph.EdgeSpec
import org.clulab.wm.eidos.graph.NodeSpec
import org.clulab.wm.eidos.graph.Quant
import org.clulab.wm.eidos.graph.TimEx
import org.clulab.wm.eidos.test.EnglishTest
import org.clulab.wm.eidos.test.TestUtils._

class TestJava11 extends EnglishTest {

  { // Document 2, Paragraph 2
    val version = System.getProperty("java.version")
    val text = """
                 |The Food and Agriculture Organization of the United Nations (FAO), the United Nations
                 |Children's Fund (UNICEF) and the World Food Programme (WFP) stressed that while the
                 |deteriorating situation coincides with an unusually long and harsh annual lean season, when families
                 |have depleted their food stocks and new harvests are not expected until August, the level of food
                 |insecurity this year is unprecedented.
               """.stripMargin
    val tester = new GraphTester(text)

    val situation = NodeSpec("deteriorating situation", Dec("deteriorating"))
    val leanSeason = NodeSpec("unusually long and harsh annual lean season", Quant("harsh"), TimEx("annual"))

    val document = ieSystem.annotate(text)
    val annotatedDocument = ieSystem.extractFromText(text)

    behavior of "TestDoc2 Paragraph 2"

    def getEntity(document: Document): String = document.sentences.head.entities.get(43)

    it should "have correct entity" taggedAs(Keith) in {
      val testerResult = tester.test(EdgeSpec(situation, Correlation, leanSeason))

      val entity = getEntity(document)
      val annotatedEntities = annotatedDocument.eidosMentions.map { eidosMention =>
        getEntity(eidosMention.odinMention.document)
      }
      val annotatedEntity = annotatedEntities.head
      val testerEntity = getEntity(tester.testResults.get(leanSeason).mention.head.document)

      println(version)
      entity should be ("SET") // Java 1.8
      annotatedEntity should be ("SET")
      testerEntity should be ("SET")
//      entity should be ("O") // Java 11
//      annotatedEntity should be ("O")
      testerResult should be(successful)
    }
  }
}
