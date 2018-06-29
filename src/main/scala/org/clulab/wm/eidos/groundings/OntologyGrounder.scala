package org.clulab.wm.eidos.groundings

import org.clulab.utils.DependencyUtils
import org.clulab.wm.eidos.mentions.EidosMention

object Aliases {
  type Grounding = Seq[(String, Double)]
  type Groundings = Map[String, OntologyGrounding]
}

case class OntologyGrounding(grounding: Aliases.Grounding = Seq.empty)

trait OntologyGrounder {
  def groundOntology(mention: EidosMention): OntologyGrounding
}

trait MultiOntologyGrounder {
  def groundOntology(mention: EidosMention): Aliases.Groundings
}

class EidosOntologyGrounder(var name: String, conceptEmbeddings: Seq[ConceptEmbedding], wordToVec: EidosWordToVec, idfOntology: Map[String, Double]) extends OntologyGrounder {

  def groundOntology(mention: EidosMention): OntologyGrounding = {
    if (mention.odinMention.matches("Entity")) { // TODO: Store this string somewhere
      val canonicalName = mention.canonicalName
      val canonicalNameParts = canonicalName.split(" +")

      // Note: computing weight of a mention token =  (i) 1 / distance-to-syn-head + (ii) idf
      val mentionTokenWeights = mkMentionTokenWeights(mention, wordToVec, idfOntology)

      OntologyGrounding(wordToVec.calculateSimilarities(canonicalNameParts, conceptEmbeddings, mentionTokenWeights))
    }
    else
      OntologyGrounding()
  }

  def mkMentionTokenWeights(mention: EidosMention, wordToVec: EidosWordToVec, idfOntology: Map[String, Double]): Map[String,Double] = {

    val sentId = mention.odinMention.sentence
    val sent = mention.odinMention.document.sentences(sentId)
    val tokenInterval = mention.odinMention.tokenInterval
    val menTokIds = tokenInterval.iterator.toArray
    // NOTE: Taking the top one
    val synHeadId = DependencyUtils.findHeadsStrict(tokenInterval, sent).head
//    val synHead = sent.getSentenceFragmentText(synHeadId, synHeadId+1)

    val menTokWeights = menTokIds.map { mTokId =>
      val token = sent.getSentenceFragmentText(mTokId, mTokId+1)
//      val wt_syn = 1.0 / math.abs (synHeadId - mTokId + 1) // Note: token distances .. not accurate .. commenting
//      println(s"computing the shortest dependency path using dep. tree ...")
      val wt_syn = 1.0 / math.abs ( sent.dependencies.get.shortestPath(mTokId, synHeadId, true).length ) // Note: getting the syntactic distance ... maybe abs() not necessary
      val wt_idf = if (idfOntology.contains(token))
                      idfOntology.get(token).get
                   else
                      0.0
      (token, wt_syn + wt_idf) // Todo: is this addition or multiplication ?
    }.toMap

    menTokWeights
  }
}

// todo: make class BranchedOntologyGrounder that extends this, and will be used along with BranchedOntology with some special-case
// methods for grounding to the diff branches...

object EidosOntologyGrounder {
  // Namespace strings for the different in-house ontologies we typically use
  val UN_NAMESPACE = "un"
  val WDI_NAMESPACE = "wdi"
  val FAO_NAMESPACE = "fao"

  def apply(domainOntology: DomainOntology, wordToVec: EidosWordToVec) = {
    val conceptEmbeddings = domainOntology.iterateOntology(wordToVec)
    val idfOntology = domainOntology.computeIDF()
    new EidosOntologyGrounder(domainOntology.name, conceptEmbeddings, wordToVec, idfOntology)
  }


//  def getConceptEmbeddings(ontologyPath: String, wordToVec: EidosWordToVec): Map[String, Seq[Double]] = {
//    val ontology = DomainOntology(FileUtils.loadYamlFromResource(ontologyPath), filterOnPos = false)
//
//    ontology.iterateOntology(wordToVec)
  }
