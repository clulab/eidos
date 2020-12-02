package org.clulab.wm.eidos.groundings

import java.time.ZonedDateTime

import org.clulab.wm.eidos.groundings.OntologyAliases._
import org.clulab.wm.eidos.groundings.grounders.PredicateTuple
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidoscommon.utils.Namer
import org.clulab.wm.ontologies.DomainOntology

object OntologyAliases {
//  type SingleOntologyGrounding = (Namer, Float)
  type MultipleOntologyGrounding = Seq[IndividualGrounding]
  // The first string is the name, something like wm or un.  The second is a branch/category.
  type OntologyGroundings = Map[String, OntologyGrounding]
}

trait IndividualGrounding {
  def name: String
  def score: Float
}
case class SingleOntologyNodeGrounding(namer: Namer, override val score: Float) extends IndividualGrounding{
  def name: String = namer.name
}
object SingleOntologyNodeGrounding {
  def apply(tuple: (Namer, Float)): SingleOntologyNodeGrounding = SingleOntologyNodeGrounding(tuple._1, tuple._2)
}
case class PredicateGrounding(predicateTuple: PredicateTuple) extends IndividualGrounding {
  def name: String = predicateTuple.name
  def score: Float = predicateTuple.score
}



case class OntologyGrounding(version: Option[String], date: Option[ZonedDateTime], grounding: MultipleOntologyGrounding = Seq.empty, branch: Option[String] = None) {
  def nonEmpty: Boolean = grounding.nonEmpty
  def take(n: Int): MultipleOntologyGrounding = grounding.take(n)
  def headOption: Option[IndividualGrounding] = grounding.headOption
  def headName: Option[String] = headOption.map(_.name)
}

trait OntologyGrounder {
  def name: String
  def domainOntology: DomainOntology
  def groundEidosMention(mention: EidosMention, topN: Option[Int], threshold: Option[Float]): Seq[OntologyGrounding]
  def groundStrings(strings: Array[String]): Seq[OntologyGrounding]
}
