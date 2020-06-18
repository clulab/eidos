package org.clulab.wm.eidos.groundings.grounders

import org.clulab.wm.eidos.groundings.ConceptEmbedding
import org.clulab.wm.eidos.groundings.ConceptPatterns
import org.clulab.wm.eidos.groundings.DomainOntology
import org.clulab.wm.eidos.groundings.EidosWordToVec
import org.clulab.wm.eidos.groundings.OntologyGrounding
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.Canonicalizer

class InterventionSieveGrounder(name: String, domainOntology: DomainOntology, wordToVec: EidosWordToVec, canonicalizer: Canonicalizer)
    extends EidosOntologyGrounder(name, domainOntology, wordToVec, canonicalizer) {

  // FIXME: gross hack
  def groundStrings(strings: Array[String]): Seq[OntologyGrounding] = {
    ???
  }

  def inBranch(s: String, branches: Seq[ConceptEmbedding]): Boolean =
    branches.exists(_.namer.name == s)

  protected lazy val conceptEmbeddingsSeq: Map[String, Seq[ConceptEmbedding]] = {
    val (interventionNodes, rest) = conceptEmbeddings.partition(_.namer.name.contains("causal_factor/interventions"))
    Map(
      InterventionSieveGrounder.INTERVENTION -> interventionNodes,
      InterventionSieveGrounder.REST -> rest
    )
  }

  protected lazy val conceptPatternsSeq: Map[String, Seq[ConceptPatterns]] = {
    val (interventionNodes, rest) = conceptPatterns.partition(_.namer.name.contains("causal_factor/interventions"))
    Map(
      InterventionSieveGrounder.INTERVENTION -> interventionNodes,
      InterventionSieveGrounder.REST -> rest
    )
  }

  def groundOntology(mention: EidosMention, topN: Option[Int] = Some(5), threshold: Option[Float] = Some(0.5f)): Seq[OntologyGrounding] = {
    if (EidosOntologyGrounder.groundableType(mention)) {
      // First check to see if the text matches a regex from the main part of the ontology,
      // if so, that is a very precise grounding and we want to use it.
      val matchedPatternsMain = nodesPatternMatched(mention.odinMention.text, conceptPatternsSeq(InterventionSieveGrounder.REST))
      if (matchedPatternsMain.nonEmpty) {
        Seq(newOntologyGrounding(matchedPatternsMain))
      }
      // Otherwise, back-off to the w2v-based approach for main branch and a sieve for interventions
      else {
        val canonicalNameParts = canonicalizer.canonicalNameParts(mention)

        // Main Portion of the ontology
        val mainConceptEmbeddings = conceptEmbeddingsSeq(InterventionSieveGrounder.REST)
        val mainSimilarities = wordToVec.calculateSimilarities(canonicalNameParts, mainConceptEmbeddings)

        // Intervention Branch
        // Only allow grounding to these nodes if the patterns match
        val ontologyPatterns = conceptPatternsSeq(InterventionSieveGrounder.INTERVENTION).flatMap(xxx => xxx.patterns.getOrElse(Array()))
        val possibleIntervention = nodePatternsMatch(mention.odinMention.text, Some(InterventionSieveGrounder.regexes ++ ontologyPatterns))
        val interventionSimilarities = if (possibleIntervention) {
          val matchedPatternsInterventions = nodesPatternMatched(mention.odinMention.text, conceptPatternsSeq(InterventionSieveGrounder.INTERVENTION))
          // If you match a node pattern, give it a score of 1.0
          if (matchedPatternsInterventions.nonEmpty)
            matchedPatternsInterventions
          else {
            val interventionConceptEmbeddings = conceptEmbeddingsSeq(InterventionSieveGrounder.INTERVENTION)

            wordToVec.calculateSimilarities(canonicalNameParts, interventionConceptEmbeddings)
          }
        }
        else
          Seq.empty
        val similarities = (mainSimilarities ++ interventionSimilarities).sortBy(-_._2).take(topN.get)

        Seq(newOntologyGrounding(similarities))
      }
    }
    else
      Seq(newOntologyGrounding())
  }
}

object InterventionSieveGrounder {
  val INTERVENTION = "interventions"
  val REST = "rest"

  val branches: Seq[String] = Seq(INTERVENTION, REST)

  val regexes = Array(
    // from ontology
    """\b(census|survey|AgSS|CFSVA)\\b""".r,
    """(?i)\b(vaccin(e|ate|ation)s?)\b""".r,
    """(?i)\b(immuniz(ations?|e))\b""".r,
    """(?i)\bextension\s+offices?""".r,
    """(?i)(tower|bunker|stave|grain)\s(silo)""".r,
    """\b(WASH)\b""".r,
    """(?i)\b(road|bridge|rail)\s(repair)""".r,
    """(IPM)(\s(practices))?""".r,
    """(?i)reports?""".r, // slight adaptation of the regex in ontology
    """\b(DDT|dicamba|round-up)\b""".r,
    """\b(IPC|FEWS|VAM)\b""".r,
    """(?i)\bearly\s+warning\s+system\b""".r,
    // humanitarian assistance
    "(?i)(intervention)".r,
    """(?i)(humanitarian)(\s|\w)+(aid|assistance)""".r,
    // provision of goods and services
    """(?i)((provision\s+of)|(distribution\s+of)|providing|provided?|distributed?)\s+(?!.*(rain|rainfall))""".r,
    """(?i)(\w)+(?<!rain|rainfall)\s+(provision|distribution|assistance)""".r,
    // infrastructure
    """(((rehabilitation|restoration|construction|repair)\s+of)|(build|restoring|constructing|repairing))""".r,
    """(disaster\s+relief|field\s+hospital|classrooms)""".r,
    """(improvement\s+of|improved)(\s|\w)+(school|infrastructure|education|facilit)""".r,
    """(temporary|relief|deployment)(\s|\w)+(camp|building|facilit|shelter|settlement)""".r,
    """(borehole\s+drilling)""".r,
    // peacekeeping and security
    """(peacekeeping|peacekeepers|peace\s+(talk|treaty)|political\s+mediation)""".r,
    """(conflict)(\s|\w)+(resolution|mediation|intervention)""".r,
    """(formal\s+mediation)""".r,
    """(ceasefire|cease\s+fire)""".r,
    """(disarm|demobiliz|reintegrat)""".r,
    // institutional support
    """((engag|interact|cooperation)(\s|\w)+with|support)(authorit|institution)""".r,
    """(support|develop|strengthen)(\s|\w)+(capacity|framework)""".r,
    """(integrat)(\s|\w)+into(\s|\w)+(policy|policies|program)""".r,
    """(capacity)(\s|\w)+building""".r,
    """(public\s+sector\s+support)""".r,
    """(invest)(\s|\w)+(in)""".r,
  )
}
