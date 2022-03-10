package org.clulab.wm.eidos.groundings.grounders.srl

import org.clulab.wm.eidos.groundings.IndividualGrounding
import org.clulab.wm.eidos.groundings.OntologyGrounding
import org.clulab.wm.eidos.utils.GroundingUtils

// TODO: Keep track of word and index?
class PredicateTuple protected (
  val theme: OntologyGrounding,
  val themeProperties: OntologyGrounding,
  val themeProcess: OntologyGrounding,
  val themeProcessProperties: OntologyGrounding
) extends IndexedSeq[OntologyGrounding] {
  val score: Float = {
    val allScores = indices.map(getScoreOpt).flatten
    GroundingUtils.noisyOr(allScores)
  }
  val labelers: Array[() => String] = Array(
    () => nameAndScore(theme),
    () => themeProperties.take(5).map(nameAndScore).mkString(", "),
    () => nameAndScore(themeProcess),
    () => themeProcessProperties.take(5).map(nameAndScore).mkString(", ")
  )

  override def length: Int = 4

  def apply(index: Int): OntologyGrounding = {
    index match {
      case 0 => theme
      case 1 => themeProperties
      case 2 => themeProcess
      case 3 => themeProcessProperties
    }
  }

  def getScoreOpt(index: Int): Option[Float] = this(index).individualGroundings.headOption.map(_.score * PredicateTuple.weights(index))

  def nameAndScore(index: Int): String = nameAndScore(this(index))

  def nameAndScore(ontologyGrounding: OntologyGrounding): String = nameAndScore(ontologyGrounding.headOption.get)

  def nameAndScore(individualGrounding: IndividualGrounding): String = s"${individualGrounding.name} (${individualGrounding.score})"

  def indexToStringOpt(index: Int): Option[String] = this(index)
      .individualGroundings
      .headOption
      .map(PredicateTuple.labels(index) + nameAndScore(_))

  override def toString: String = indices
      .map(indexToStringOpt)
      .flatten
      .mkString("\n")

  val name: String = {
    var hasSome = false

    def someIf(grounding: OntologyGrounding, label: String, separator: String, labeler: () => String): Option[String] = {
      if (grounding.isEmpty) None
      else {
        val string = (if (hasSome) separator else "") + label + labeler()
        hasSome = true
        Some(string)
      }
    }

    indices
        .map { index =>
          someIf(this(index), PredicateTuple.labels(index), PredicateTuple.separators(index), labelers(index))
        }
        .flatten
        .mkString("")
  }
}

object PredicateTuple {
  // Compositional Ontology Branches
  val CONCEPT = "concept"
  val PROCESS = "process"
  val PROPERTY = "property"

  val labels = Array(
    "THEME: ",
    "Theme properties: ",
    "THEME PROCESS: ",
    "Process properties: "
  )
  val separators = Array("; ", ", ", "; ", ", ")
  val weights = Array(1f, 0.6f, 1f, 0.6f)

  def apply(
    theme: OntologyGrounding,
    themeProperties: OntologyGrounding,
    themeProcess: OntologyGrounding,
    themeProcessProperties: OntologyGrounding
  ): PredicateTuple = new PredicateTuple(
    theme.filterSlots(CONCEPT),
    themeProperties.filterSlots(PROPERTY),
    themeProcess.filterSlots(PROCESS),
    themeProcessProperties.filterSlots(PROPERTY)
  )

  def apply(ontologyGroundings: Array[OntologyGrounding]): PredicateTuple = {
    require(ontologyGroundings.length == 4)
    apply(ontologyGroundings(0), ontologyGroundings(1), ontologyGroundings(2), ontologyGroundings(3))
  }

  def apply(
      ontologyGrounding1Opt: Option[OntologyGrounding],
      ontologyGrounding2Opt: Option[OntologyGrounding],
      ontologyGrounding3Opt: Option[OntologyGrounding],
      ontologyGrounding4Opt: Option[OntologyGrounding],
      emptyOntologyGrounding: OntologyGrounding
  ): PredicateTuple = {
    apply(
      ontologyGrounding1Opt.getOrElse(emptyOntologyGrounding),
      ontologyGrounding2Opt.getOrElse(emptyOntologyGrounding),
      ontologyGrounding3Opt.getOrElse(emptyOntologyGrounding),
      ontologyGrounding4Opt.getOrElse(emptyOntologyGrounding)
    )
  }

  def toPredicateTupleOptWithBranch(ontologyGrounding: OntologyGrounding, branch: String, emptyOntologyGrounding: OntologyGrounding): Option[PredicateTuple] = {
    if (ontologyGrounding.isEmpty) None
    else
      branch match {
        case CONCEPT =>
          Some(PredicateTuple(ontologyGrounding, emptyOntologyGrounding, emptyOntologyGrounding, emptyOntologyGrounding))
        case PROCESS =>
          Some(PredicateTuple(emptyOntologyGrounding, emptyOntologyGrounding, ontologyGrounding, emptyOntologyGrounding))
        case _ => None
      }
  }

  def toPredicateTupleOpt(ontologyGrounding: OntologyGrounding, emptyOntologyGrounding: OntologyGrounding): Option[PredicateTuple] = {
    if (ontologyGrounding.individualGroundings.isEmpty)
      None
    else {
      val branch = ontologyGrounding.individualGroundings.head.branchOpt.get

      toPredicateTupleOptWithBranch(ontologyGrounding, branch, emptyOntologyGrounding)
    }
  }
}
