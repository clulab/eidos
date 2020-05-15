package org.clulab.wm.eidos.serialization.webapp

import com.typesafe.config.Config
import org.clulab.processors.Document
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import play.api.libs.json._

object HomeController extends App {
  println("[EidosSystem] Initializing the EidosSystem ...")
  val eidosConfig: Config = EidosSystem.defaultConfig
  val ieSystem: EidosSystem = new EidosSystem(eidosConfig)
  println("[EidosSystem] Completed Initialization ...")

  {
    println("[EidosSystem] Priming the EidosSystem ...")
    // Modernize this without the fake date?
    ieSystem.extractFromText("In 2014 drought caused a famine in Ethopia.", cagRelevantOnly = true, Some("2019-08-09"))
    println("[EidosSystem] Completed Priming ...")
  }

  // Need name of output file
  def processText(text: String, cagRelevantOnly: Boolean): Unit = {
    println(s"Processing sentence : $text")
    val annotatedDocument = ieSystem.extractFromText(text, cagRelevantOnly = cagRelevantOnly)
    println("DONE .... ")
    process(annotatedDocument, cagRelevantOnly,"webapp.html")
  }

  def process(annotatedDocument: AnnotatedDocument, cagRelevantOnly: Boolean, fileName: String): Unit = {
    val text = annotatedDocument.document.text.get
    val json = processAnnotatedDocument(text, annotatedDocument).toString
    val oldIndexHtml = FileUtils.getTextFromFile("./webapp/app/views/index.scala.html")
    val webhead = FileUtils.getTextFromResource("/org/clulab/wm/eidos/webapp/webhead.html")
        .replace("$json", json)
        .replace("$cagRelevantOnly", cagRelevantOnly.toString)
    val headStart = oldIndexHtml.indexOf("<head>")
    val headStop = oldIndexHtml.indexOf("</head>")
    val newIndexHtml = oldIndexHtml.substring(0, headStart) + webhead + oldIndexHtml.substring(headStop)

    FileUtils.printWriterFromFile(fileName).autoClose { printWriter =>
      printWriter.println(newIndexHtml)
    }
  }

  // Later assume text is already there.
  def processAnnotatedDocument(text: String, annotatedDocument: AnnotatedDocument): JsValue = {
    val doc = annotatedDocument.document
    // This grounding should have taken place already.
    val eidosMentions = annotatedDocument.eidosMentions.sortBy(m => (m.odinMention.sentence, m.getClass.getSimpleName, m.odinMention.start))
    val groundedEntities = GroundedEntity.groundEntities(ieSystem, eidosMentions)
    println("Found mentions (in mkJson):")
    val json = mkJson(text, doc, eidosMentions, groundedEntities)

    json
  }

  def mkJson(text: String, doc: Document, eidosMentions: Seq[EidosMention], groundedEntities: Vector[GroundedEntity] ): JsValue = {
    val odinMentions = eidosMentions.map(_.odinMention)
    val timExs = ieSystem.components.timeNormFinderOpt.map(_.getTimExs(odinMentions, doc.sentences))
    val geoPhraseIDs = ieSystem.components.geoNormFinderOpt.map(_.getGeoPhraseIDs(odinMentions, doc.sentences))
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

  val json = processText("The recent drought caused widespread famine in Ethoipia.", true)

  println(json)
}
