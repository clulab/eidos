package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.text.{AntiNodeSpec, NodeSpec}

class TestOntologyGrounder extends Test {

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

    val tester = new Tester(text)

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
  }
}
