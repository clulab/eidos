package org.clulab.wm.eidos.groundings

import org.clulab.odin.EventMention
import org.clulab.wm.eidos.graph._
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.utils.Canonicalizer
import org.clulab.wm.eidos.utils.FileUtils

class TestOntologyGrounder extends EnglishTest {

  {
    val text =
      """
        |New Zealand is a nation.
        |Australia is just next door.
        |The FAO is active there.
        |First is a good place to be.
        |America causes soybean production.
        |Paper causes cuts.
        |A pair causes a two.
      """.stripMargin

    val tester = new GraphTester(text)

    val newZealand = AntiNodeSpec("New Zealand") // LOCATION (in two words)
    val australia = AntiNodeSpec("Australia") // LOCATION (in one word)
    val fao = AntiNodeSpec("FAO") // ORGANIZATION
    val first = AntiNodeSpec("First") // ORDINAL
    val america = AntiNodeSpec("America") // One involved in an event
    val paper = AntiNodeSpec("Paper") // Stopword
    val two = AntiNodeSpec("Two") // POS is CD

    behavior of "Ontology Grounder"

    it should "filter a two-word location" taggedAs(Somebody) in {
      tester.test(newZealand)
    }
    it should "filter a one-word location" taggedAs(Somebody) in {
      tester.test(australia)
    }
    it should "filter an organization" taggedAs(Somebody) in {
      tester.test(fao)
    }
    it should "filter an ordinal" taggedAs(Somebody) in {
      tester.test(first)
    }
    it should "filter a location involved in an event" taggedAs(Somebody) in {
      tester.test(america)
    }
    it should "filter a stopword" taggedAs(Somebody) in {
      tester.test(paper)
    }
    it should "filter the cardinal POS" taggedAs(Somebody) in {
      tester.test(paper)
    }

    ignore should "reground properly" taggedAs(Keith) in {
      // Run this with grounding on or else ignore!
      val annotatedDocument = ieSystem.extractFromText("Trade with Sudan, South Sudan's second most important trading partner, has decreased significantly since independence.", false)
      val odinMentions = annotatedDocument.odinMentions.filter { odinMention => odinMention.matches("Entity") } // must be groundable
      val odinMention = odinMentions.head
      val eidosMentions = annotatedDocument.eidosMentions.filter { eidosMention => eidosMention.odinMention.eq(odinMention) }
      val eidosMention = eidosMentions.head
      val unGrounding = eidosMention.grounding("un").grounding
      val grounding = unGrounding.map { case (namer, value) => (namer.name, value) }

      val ontologyYaml = FileUtils.getTextFromResource("/org/clulab/wm/eidos/english/ontologies/un_ontology.yml")
      val ontologyHandler = ieSystem.components.ontologyHandler
      val text = odinMention.text
      val canonicalName = eidosMention.canonicalName

      def reground(text: String) = {
        val name = "test"
        val regrounding = ontologyHandler.reground(name = name, ontologyYaml = ontologyYaml, canonicalNames = Seq(text))

        regrounding
      }

      val textRegroundings = reground(text)(0).toSeq
      val canonicalRegroundings = reground(canonicalName)(0).toSeq

      grounding should be (canonicalRegroundings)
      grounding should not be (textRegroundings)
    }
  }
}
