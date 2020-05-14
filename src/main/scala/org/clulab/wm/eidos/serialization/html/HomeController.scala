package org.clulab.wm.eidos.serialization.html

import com.typesafe.config.Config
import org.clulab.processors.Document
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.mentions.EidosMention
import play.api.libs.json._

object HomeController extends App {
  println("[EidosSystem] Initializing the EidosSystem ...")
  val eidosConfig: Config = EidosSystem.defaultConfig
  val ieSystem: EidosSystem = new EidosSystem(eidosConfig)
  println("[EidosSystem] Completed Initialization ...")

  {
    println("[EidosSystem] Priming the EidosSystem ...")
    ieSystem.extractFromText("In 2014 drought caused a famine in Ethopia.", cagRelevantOnly = true, Some("2019-08-09"))
    println("[EidosSystem] Completed Priming ...")
  }

  val json = processText("The recent drought caused widespread famine in Ethoipia.", false)
  println(json)

  def processText(text: String, cagRelevantOnly: Boolean): JsValue = {
    println(s"Processing sentence : $text" )
    val annotatedDocument = ieSystem.extractFromText(text, cagRelevantOnly = cagRelevantOnly)
    println("DONE .... ")
    val doc = annotatedDocument.document
    // This grounding should have taken place already.
    val eidosMentions = annotatedDocument.eidosMentions.sortBy(m => (m.odinMention.sentence, m.getClass.getSimpleName, m.odinMention.start))
    val groundedEntities = GroundedEntity.groundEntities(ieSystem, eidosMentions)
    val json = mkJson(text, doc, eidosMentions, groundedEntities)

    json
  }

  def mkJson(text: String, doc: Document, eidosMentions: Seq[EidosMention], groundedEntities: Vector[GroundedEntity] ): JsValue = {
    println("Found mentions (in mkJson):")
    val odinMentions = eidosMentions.map(_.odinMention)
    val timExs = ieSystem.components.timeNormFinderOpt.map(_.getTimExs(odinMentions, doc.sentences))
    val geoPhraseIDs = ieSystem.components.geoNormFinderOpt.map(_.getGeoPhraseIDs(odinMentions, doc.sentences))
    val sent = doc.sentences.head

    val syntaxObj = SyntaxObj.mkJsonFromSyntax(doc, text)
    val eidosObj = EidosObj.mkJsonForEidos(text, sent, odinMentions, timExs, geoPhraseIDs)
    val groundedObj = GroundedObj.mkGroundedObj(groundedEntities, eidosMentions, timExs, geoPhraseIDs)
    val parseObj = ParseObj.mkParseObj(doc)

    Json.obj(fields =
      "syntax" -> syntaxObj,
      "eidosMentions" -> eidosObj,
      "groundedAdj" -> groundedObj,
      "parse" -> parseObj
    )
  }
}
