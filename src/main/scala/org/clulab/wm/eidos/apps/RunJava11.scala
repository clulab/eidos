package org.clulab.wm.eidos.apps

import org.clulab.processors.Document
import org.clulab.wm.eidos.EidosSystem

object RunJava11 extends App {

  def trace[T](name: String)(f: => T): T = {
    val result = f
    println(s"$name = $result")
    result
  }

  def getEntity(document: Document): String = document.sentences.head.entities.get(43)

  { // Document 2, Paragraph 2
    val version = trace("version"){ System.getProperty("java.version") }
    val text = """
                 |The Food and Agriculture Organization of the United Nations (FAO), the United Nations
                 |Children's Fund (UNICEF) and the World Food Programme (WFP) stressed that while the
                 |deteriorating situation coincides with an unusually long and harsh annual lean season, when families
                 |have depleted their food stocks and new harvests are not expected until August, the level of food
                 |insecurity this year is unprecedented.
               """.stripMargin

    val ieSystem = new EidosSystem()
    val document = ieSystem.annotate(text)
    val annotatedDocument = ieSystem.extractFromText(text)

    val entity = getEntity(document)
    val annotatedEntities = annotatedDocument.eidosMentions.map { eidosMention =>
      getEntity(eidosMention.odinMention.document)
    }
    val annotatedEntity = annotatedEntities.head

    assert(entity == "SET")
    assert(annotatedEntity == "SET")
  }
}
