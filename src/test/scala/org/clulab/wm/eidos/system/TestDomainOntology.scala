package org.clulab.wm.eidos.system

import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.wm.eidos.test.TestUtils._
import org.clulab.wm.eidos.utils.{FAOOntology, ToyOntology, UNOntology, WDIOntology}

class TestDomainOntology extends Test {
  
  val proc = new FastNLPProcessor()

  // Do test classes have access to main resources?  Yes, apparently.
  // These paths must be coordinated with default values in EidosSystem.

  behavior of "toy ontology"
  it should "not throw exception when loading" in {
    ToyOntology("toy", "/org/clulab/wm/eidos/ontologies/ontology.yml", proc)
  }

  behavior of "un ontology"
  it should "not throw exception when loading" in {
    UNOntology("un", "/org/clulab/wm/eidos/ontologies/un_ontology.yml", proc)
  }

  behavior of "fao ontology"
  it should "not throw exception when loading" in {
    FAOOntology("fao", "/org/clulab/wm/eidos/ontologies/fao_variable_ontology.yml", proc)
  }

  behavior of "wdi ontology"
  it should "not throw exception when loading" in {
    WDIOntology("wdi", "/org/clulab/wm/eidos/ontologies/wdi_ontology.yml", proc)
  }
}
