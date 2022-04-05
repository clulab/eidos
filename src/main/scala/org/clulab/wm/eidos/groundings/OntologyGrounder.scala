package org.clulab.wm.eidos.groundings

import org.clulab.wm.eidos.groundings.OntologyAliases._
import org.clulab.wm.eidos.groundings.grounders.srl.PredicateTuple
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidoscommon.utils.Namer
import org.clulab.wm.ontologies.DomainOntology

import java.time.ZonedDateTime

object OntologyAliases {
  type IndividualGroundings = Seq[IndividualGrounding]
  // The first string is the name, something like "wm" or "un".  The second is a branch/category.
  type OntologyGroundingMap = Map[String, OntologyGrounding]
}

trait IndividualGrounding {
  def name: String
  def score: Float
  def negScoreOpt: Option[Float] = None
  def branchOpt: Option[String] = name.split('/').lift(1)
}

case class OntologyNodeGrounding(namer: Namer, override val score: Float, override val negScoreOpt: Option[Float] = None) extends IndividualGrounding{
  def name: String = namer.getName
}

object OntologyNodeGrounding {
  def apply(tuple: (Namer, Float)): OntologyNodeGrounding = OntologyNodeGrounding(tuple._1, tuple._2)
}

case class PredicateGrounding(predicateTuple: PredicateTuple) extends IndividualGrounding {
  def name: String = predicateTuple.name
  def score: Float = predicateTuple.score
  override def toString(): String = predicateTuple.toString() + s" Total: $score"
}

case class OntologyGrounding(versionOpt: Option[String], dateOpt: Option[ZonedDateTime], individualGroundings: IndividualGroundings = Seq.empty, branchOpt: Option[String] = None) {
  def nonEmpty: Boolean = individualGroundings.nonEmpty
  def isEmpty: Boolean = individualGroundings.isEmpty
  def take(n: Int): IndividualGroundings = individualGroundings.take(n)
  def headOption: Option[IndividualGrounding] = individualGroundings.headOption
  def headName: Option[String] = headOption.map(_.name)
  // Discard the head grounding and return a copy with the tail.
  def dropFirst(): OntologyGrounding = OntologyGrounding(versionOpt, dateOpt, individualGroundings.drop(1), branchOpt)
  def filterSlots(slot: String): OntologyGrounding = OntologyGrounding(versionOpt, dateOpt, individualGroundings.filter(_.branchOpt == Some(slot)))
}

trait OntologyGrounder {
  def name: String
  def domainOntology: DomainOntology
  def groundEidosMention(mention: EidosMention, topN: Option[Int], threshold: Option[Float]): Seq[OntologyGrounding]
  def groundStrings(strings: Array[String]): Seq[OntologyGrounding]
}
