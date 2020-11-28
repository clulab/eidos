package org.clulab.wm.eidos.utils

import org.clulab.embeddings.{WordEmbeddingMap => Word2Vec}
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.apps.OntologyMapper.{mostSimilar, mostSimilarIndicators}
import org.clulab.wm.eidos.groundings.{ConceptEmbedding, DomainOntology, OntologyHandler}
import org.clulab.wm.eidos.groundings.grounders.EidosOntologyGrounder
import upickle.default._
import upickle.default.{ReadWriter, macroRW}


object MaaSUtils {
  def mapOntology(reader: EidosSystem, ontologyName: String, ontologyString: String, topN: Int = 10): String = {
    val grounders: Seq[EidosOntologyGrounder] = reader.components.ontologyHandlerOpt.get.ontologyGrounders.collect{ case g: EidosOntologyGrounder => g }
    // For purposes of this app, it is assumed that the primary grounder exists.
    val primaryGrounder = grounders.find { grounder => grounder.name == EidosOntologyGrounder.PRIMARY_NAMESPACE }.get
    val primaryConceptEmbeddings = primaryGrounder.conceptEmbeddings
    val primaryKeys = primaryConceptEmbeddings.map(_.namer.name)
    val canonicalizer = new Canonicalizer(reader.components.stopwordManagerOpt.get, reader.components.procOpt.get.getTagSet)
    val providedOntology = OntologyHandler.mkDomainOntologyFromYaml(
      ontologyName,
      ontologyString,
      reader.components.procOpt.get,
      canonicalizer,
      includeParents = true
    )
    val grounder = EidosOntologyGrounder(
      ontologyName,
      providedOntology,
      reader.components.ontologyHandlerOpt.get.wordToVec,
      canonicalizer
    )
    val concepts = grounder.conceptEmbeddings

    // Seq(Concept:String, Seq(Indicator: String, Score: Float))
    val mostSimilarSorted =
      for {
        (conceptName, sortedIndicators)  <- mostSimilarIndicators(primaryConceptEmbeddings, concepts, topN, reader)
        matchedConcepts = sortedIndicators.map(indAndScore => ConceptMatch(indAndScore._1, indAndScore._2))
      } yield ConceptIndicators(conceptName, Alignments(matchedConcepts))
    write(mostSimilarSorted)
  }

  def mapNodeToPrimaryConcepts(reader: EidosSystem, data: String, topN: Int = 10): String = {
    val json = ujson.read(data)
    val node = json("name").str
    val examples = json("examples").arr.map(_.toString)
    // create a bag of words, and sanitize them
    val sanitizedExampleBag = examples.flatMap(_.split("\\s+")).map(w => Word2Vec.sanitizeWord(w))
    val w2v = reader.components.ontologyHandlerOpt.get.wordToVec
    // average the word embeddings
    val embedding = w2v.makeCompositeVector(sanitizedExampleBag)
    // convert to a ConceptEmbedding
    val conceptEmbed = ConceptEmbedding(new PassThruNamer(node), embedding)

    val grounders: Seq[EidosOntologyGrounder] = reader.components.ontologyHandlerOpt.get.ontologyGrounders.collect{ case g: EidosOntologyGrounder => g }
    // For purposes of this app, it is assumed that the primary grounder exists.
    val primaryGrounder = grounders.find { grounder => grounder.name == EidosOntologyGrounder.PRIMARY_NAMESPACE }.get
    val primaryConceptEmbeddings = primaryGrounder.conceptEmbeddings
    val mostSimilarConcepts = mostSimilar(conceptEmbed, primaryConceptEmbeddings, topN, reader, 1.0f, 0.0f)

    write(Alignments(mostSimilarConcepts.map(c => ConceptMatch(c._1, c._2))))
  }
}


case class ConceptIndicators(concept: String, indicators: Alignments)
object ConceptIndicators {
  implicit val rw: ReadWriter[ConceptIndicators] = macroRW
}

case class Alignments(conceptMatches: Seq[ConceptMatch])
object Alignments {
  implicit val rw: ReadWriter[Alignments] = macroRW
}

case class ConceptMatch(concept: String, score: Float)
object ConceptMatch {
  implicit val rw: ReadWriter[ConceptMatch] = macroRW
}