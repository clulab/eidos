package org.clulab.wm.discovery.apps

import org.clulab.utils.Files
import org.clulab.wm.discovery.CdrDocument
import org.clulab.wm.discovery.ConceptDiscovery
import org.clulab.wm.discovery.ConceptSink
import org.clulab.wm.discovery.ConceptSource

object DiscoverAndRankConceptsApp extends App {
  val inputDir = args(0)
  val thresholdFrequency = args(1).toDouble
  val thresholdSimilarity = args(2).toDouble
  val topPick = args(3).toInt
  // This goes last, even though not used last, because it is optional.
  val sentenceThresholdOpt = args.lift(4).map(_.toDouble)

  val conceptDiscovery = new ConceptDiscovery()
  val files = Files.findFiles(inputDir, "json").take(10)
  val cdrDocuments = files.flatMap { file =>
    val conceptSource = ConceptSource(file)
    val docId = conceptSource.getIdOpt.get
    val scoredSentences = conceptSource.getScoredSentences

    // Things elsewhere seem to require at least some text and scored sentences.
    if (conceptSource.text.nonEmpty && scoredSentences.nonEmpty)
      Some(CdrDocument(docId, scoredSentences))
    else
      None
  }
  val concepts = conceptDiscovery.discoverConcepts(cdrDocuments, sentenceThresholdOpt)
  val rankedConcepts = conceptDiscovery.rankConcepts(concepts, thresholdFrequency, thresholdSimilarity, topPick)
  val conceptSink = new ConceptSink(rankedConcepts)

  conceptSink.printJson()
}
