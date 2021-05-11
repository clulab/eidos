package org.clulab.wm.discovery.apps

import edu.stanford.nlp.ling.Word
import org.clulab.utils.Files
import org.clulab.wm.discovery.CdrDocument
import org.clulab.wm.discovery.ConceptDiscovery
import org.clulab.wm.discovery.ScoredSentence
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.json4s.DefaultFormats
import org.json4s.JArray
import org.json4s.JField
import org.json4s.JObject
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

import java.io.File

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

  // Maybe json
  rankedConcepts.foreach(println)
  [
    concept: {
      phrase:
      locations: [
        docId: "",
        sentence:
      ]
    }
    saliency:
  ]
}

class ConceptSource(cdr: JValue) {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  val text: String = (cdr \ "extracted_text").extractOpt[String].getOrElse("")
  val idOpt: Option[String] = (cdr \ "document_id").extractOpt[String]
  // If there are no concepts, the result is an empty Seq rather than None.
  val annotations = (cdr \ "annotations").extractOrElse[JArray](new JArray(List.empty[JValue])).arr
  val keySentenceAnnotation = annotations.find { jValue =>
    (jValue \ "label").extract[String] == "qntfy-key-sentence-annotator"
  }.getOrElse(new JObject(List.empty[JField]))
  val content = (keySentenceAnnotation \ "content").extractOrElse[JArray](new JArray(List.empty[JValue])).arr
  val scoredSentences = content.flatMap { jValue =>
    val offsetStart = (jValue \ "offset_start").extract[Int]
    val offsetEnd = (jValue \ "offset_end").extract[Int]
    val phrase = (jValue \ "value").extract[String]
    val score = (jValue \ "score").extract[Double]

    // We need each phrase to amount to at least one sentence in processors.
    // That doesn't happen when the phrase is empty, or probably under other conditions.
    if (phrase.trim.nonEmpty)
      Some(ScoredSentence(phrase, offsetStart, offsetEnd, score))
    else
      None
  }

  def getText: String = text

  def getIdOpt: Option[String] = idOpt

  def getScoredSentences: Seq[ScoredSentence] = scoredSentences
}

object ConceptSource {

  def apply(file: File): ConceptSource = apply(FileUtils.getTextFromFile(file))

  def apply(json: String): ConceptSource = apply(JsonMethods.parse(json))

  def apply(jValue: JValue): ConceptSource = new ConceptSource(jValue)
}
