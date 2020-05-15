package org.clulab.wm.eidos.serialization.web

import com.typesafe.config.Config
import org.clulab.processors.Document
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.DomainParams
import play.api.libs.json._

class WebSerializer(eidosSystem: EidosSystem, eidosConfig: Config) {
  val adjectiveGrounder = eidosSystem.components.adjectiveGrounder
  val domainParams = DomainParams.fromConfig(eidosConfig)
  val entityGrounder = new EntityGrounder(adjectiveGrounder, domainParams)
  val timeNormFinderOpt = eidosSystem.components.timeNormFinderOpt
  val geoNormFinderOpt = eidosSystem.components.geoNormFinderOpt

  def serialize(annotatedDocument: AnnotatedDocument, cagRelevantOnly: Boolean, fileName: String): Unit = {
    val text = annotatedDocument.document.text.get
    val json = processAnnotatedDocument(text, annotatedDocument).toString
    val oldIndexHtml = FileUtils.getTextFromFile("./webapp/app/views/index.scala.html")
    val webhead = FileUtils.getTextFromResource("/org/clulab/wm/eidos/webserializer/webhead.html")
        .replace("$json", json)
        .replace("$cagRelevantOnly", cagRelevantOnly.toString)
    val headStart = oldIndexHtml.indexOf("<head>")
    val headStop = oldIndexHtml.indexOf("</head>")
    val newIndexHtml = oldIndexHtml.substring(0, headStart) + webhead + oldIndexHtml.substring(headStop)

    FileUtils.printWriterFromFile(fileName).autoClose { printWriter =>
      printWriter.println(newIndexHtml)
    }
  }

  def processAnnotatedDocument(text: String, annotatedDocument: AnnotatedDocument): JsValue = {
    val doc = annotatedDocument.document
    val eidosMentions = annotatedDocument.eidosMentions.sortBy(m => (m.odinMention.sentence, m.getClass.getSimpleName, m.odinMention.start))
    // An adjective grounding, perhaps not this one, should have taken place already.
    val groundedEntities = entityGrounder.groundEntities(eidosMentions)
    val json = mkJson(text, doc, eidosMentions, groundedEntities)

    json
  }

  def mkJson(text: String, doc: Document, eidosMentions: Seq[EidosMention], groundedEntities: Seq[GroundedEntity] ): JsValue = {
    val odinMentions = eidosMentions.map(_.odinMention)
    val timExs = timeNormFinderOpt.map(_.getTimExs(odinMentions, doc.sentences))
    val geoPhraseIDs = geoNormFinderOpt.map(_.getGeoPhraseIDs(odinMentions, doc.sentences))
    val sent = doc.sentences.head

    val syntaxJson = new SyntaxObj(doc, text).mkJson
    val eidosJson = new EidosObj(text, sent, odinMentions, timExs, geoPhraseIDs).mkJson
    val groundedHtml = new GroundedObj(groundedEntities, eidosMentions, timExs, geoPhraseIDs).mkHtml
    val parseHtml = new ParseObj(doc).mkHtml

    Json.obj(fields =
      "syntax" -> syntaxJson,
      "eidosMentions" -> eidosJson,
      "groundedAdj" -> groundedHtml,
      "parse" -> parseHtml
    )
  }
}
