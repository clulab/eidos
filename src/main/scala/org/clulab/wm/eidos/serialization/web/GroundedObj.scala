package org.clulab.wm.eidos.serialization.web

import org.clulab.wm.eidos.context.GeoNormFinder
import org.clulab.wm.eidos.context.GeoPhraseID
import org.clulab.wm.eidos.context.TimEx
import org.clulab.wm.eidos.context.TimeNormFinder
import org.clulab.wm.eidos.groundings.GroundingUtils
import org.clulab.wm.eidos.groundings.grounders.EidosOntologyGrounder
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.DisplayUtils

class GroundedObj(groundedEntities: Seq[GroundedEntity], mentions: Seq[EidosMention], time: Option[Array[Seq[TimEx]]], location: Option[Array[Seq[GeoPhraseID]]]) {

  def tab:String = "&nbsp;&nbsp;&nbsp;&nbsp;"

  def mkHtml: String = {

    var objectToReturn = ""

    if (groundedEntities.nonEmpty) {
      objectToReturn += "<h2>Grounded Concepts:</h2>"

      // Make the string for each grounded entity
      val toPrint = for (grounding <- groundedEntities) yield {
        val sentence = grounding.sentence
        val quantifier = grounding.quantifier
        val groundedEntity = grounding.entity
        val predictedDelta = grounding.predictedDelta
        val mean = grounding.mean
        val stdev = grounding.stdev
        var stringToYield = s"${tab}Sentence: $sentence"

        stringToYield += s"<br>${tab}Entity: $groundedEntity"
        stringToYield += s"<br>${tab}Quantifier: $quantifier"
        if (predictedDelta.isDefined && mean.isDefined && stdev.isDefined)
          stringToYield += s"<br>${tab}Predicted delta = ${"%3.3f".format(predictedDelta.get)} (with typical mean=${mean.get} and stdev=${stdev.get})"
        stringToYield += "<br>"
        stringToYield
      }

      toPrint.foreach(str => objectToReturn += s"<br>$str")
    }

    else
      objectToReturn += ""

    // TimeExpressions
    val timeMentions = TimeNormFinder.getTimExs(mentions.map(_.odinMention))
    if (timeMentions.nonEmpty) {
      objectToReturn += "<h2>Found TimeExpressions:</h2>"
      objectToReturn += s"${DisplayUtils.webAppTimeExpressions(timeMentions)}"
    }

    // GeoLocations
    val locationMentions = GeoNormFinder.getGeoPhraseIDs(mentions.map(_.odinMention))
    if (locationMentions.nonEmpty) {
      objectToReturn += "<h2>Found GeoLocations:</h2>"
      objectToReturn += s"${DisplayUtils.webAppGeoLocations(locationMentions)}"
    }

    // Concepts
    val entities = mentions.filter(_.odinMention matches "Entity")
    if (entities.nonEmpty){
      objectToReturn += "<h2>Found Concepts:</h2>"
      for (entity <- entities) {
        objectToReturn += s"${DisplayUtils.webAppMention(entity.odinMention)}"
        // If the primary groundings are available, let's print them too...
        val groundingStringOpt = GroundingUtils.getGroundingsStringOpt(entity, EidosOntologyGrounder.PRIMARY_NAMESPACE, 5, s"<br>${DisplayUtils.htmlTab}${DisplayUtils.htmlTab}")
        if (groundingStringOpt.isDefined) {
          objectToReturn += s"${DisplayUtils.htmlTab}OntologyLinkings:<br>${DisplayUtils.htmlTab}${DisplayUtils.htmlTab}"
          objectToReturn +=  groundingStringOpt
          objectToReturn += "<br><br>"
        }
      }
    }

    // Relations
    val events = mentions.filter(_.odinMention matches "Event")
    if (events.nonEmpty) {
      objectToReturn += s"<h2>Found Relations:</h2>"
      for (event <- events) {
        objectToReturn += s"${DisplayUtils.webAppMention(event.odinMention)}"
      }
    }

    //    if(causalEvents.size > 0) {
    //      objectToReturn += s"<br><br>EntityLinking Events<br>"
    //      for (ce <- causalEvents) {
    //        objectToReturn += s"${tab} Trigger : ${ce._1}<br>${tab} Arguments:<br>"
    //        for (arg <-  ce._2) {
    //          objectToReturn += s"${tab}${tab}${arg._1} => ${arg._2}<br>"
    //        }
    //      }
    //    }

    objectToReturn += "<br>"
    objectToReturn
  }
}
