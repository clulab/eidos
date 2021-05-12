package org.clulab.wm.discovery

import org.json4s.JArray
import org.json4s.JValue
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods

class ConceptSink(rankedConcepts: Seq[RankedConcept]) {

  def toJValue: JValue = {
    val jArray = new JArray(
      rankedConcepts.toList.map { rankedConcept =>
        ("concept" ->
          ("phrase" -> rankedConcept.concept.phrase) ~
          ("locations" -> new JArray( {
            val documendIdsAndSentenceIndexes = rankedConcept.concept.documentLocations.toList.map { documentLocation =>
              val Array(documentId, sentenceIndex) = documentLocation.split(':')
              (documentId, sentenceIndex.toInt)
            }.sortBy(_._2)
            val jObjects = documendIdsAndSentenceIndexes.map { case (documentId, sentenceIndex) =>
              ("document_id" -> documentId) ~
              ("sentence_index" -> sentenceIndex)
            }

            jObjects
          }))
        ) ~
        ("saliency" -> rankedConcept.saliency)
      }
    )

    jArray
  }

  def printJson(): Unit = {
    val jValue = toJValue
    val json = JsonMethods.pretty(jValue)

    println(json)
  }
}
