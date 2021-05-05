package org.clulab.wm.discovery

import java.nio.file.Path

case class Concept(phrase: String, frequency: Int, documentLocations: Set[String])

case class RankedConcept(concept: Concept, saliency: Double)

class ConceptDiscovery {

  def discoverConcepts(documents: Seq[Path]): Set[Concept] = ???

  def rankConcepts(concepts: Set[Concept]): Seq[RankedConcept] = ???

}
