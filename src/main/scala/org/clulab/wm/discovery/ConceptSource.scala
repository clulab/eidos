package org.clulab.wm.discovery

import org.json4s.DefaultFormats
import org.json4s.JArray
import org.json4s.JField
import org.json4s.JObject
import org.json4s.JValue
import org.json4s.jackson.JsonMethods

import java.io.File
import java.nio.charset.StandardCharsets
import scala.io.Source

class ConceptSource(cdr: JValue) {
  implicit val formats: DefaultFormats.type = org.json4s.DefaultFormats

  val text: String = (cdr \ "extracted_text").extractOpt[String].getOrElse("")
  val idOpt: Option[String] = (cdr \ "document_id").extractOpt[String]
  val scoredSentences: Seq[ScoredSentence] = {
    // If there are no concepts, the result is an empty Seq rather than None.
    val annotations: Seq[JValue] = (cdr \ "annotations").extractOrElse[JArray](new JArray(List.empty[JValue])).arr
    val keySentenceAnnotation: JValue = annotations.find { jValue =>
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

    scoredSentences
  }

  def getText: String = text

  def getIdOpt: Option[String] = idOpt

  def getScoredSentences: Seq[ScoredSentence] = scoredSentences
}

object ConceptSource {

  def apply(file: File): ConceptSource = {
    val source = Source.fromFile(file, StandardCharsets.UTF_8.toString)
    val text = source.mkString

    source.close()
    apply(text)
  }

  def apply(json: String): ConceptSource = apply(JsonMethods.parse(json))

  def apply(jValue: JValue): ConceptSource = new ConceptSource(jValue)
}
