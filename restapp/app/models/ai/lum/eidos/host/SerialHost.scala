package ai.lum.eidos.host

import ai.lum.eidos.text.EidosText
import com.typesafe.config.Config
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.attachments.LocationDocumentAttachment
import org.clulab.wm.eidos.document.attachments.TitleDocumentAttachment
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.json4s.JValue

class SerialHost(prime: Boolean) extends EidosHost {
  val eidosConfig: Config = EidosSystem.defaultConfig
  val adjectiveGrounder: EidosAdjectiveGrounder = {
    val domainConfig: Config = eidosConfig.getConfig("adjectiveGrounder")
    EidosAdjectiveGrounder.fromConfig(domainConfig)
  }
  lazy val eidosSystem: EidosSystem = {
    println("Starting up eidos...")
    new EidosSystem(eidosConfig)
  }

  def prepare(): Unit = {
    val annotatedDocument =
      eidosSystem.extractFromText("In 2014 drought caused a famine in Ethopia.", cagRelevantOnly = true, Some("2019-08-09"))
    val corpus = new JLDCorpus(annotatedDocument)

    corpus.serialize(adjectiveGrounder)
  }

  def process(eidosText: EidosText): JValue = {
    val annotatedDocument = eidosSystem.extractFromTextWithDct(eidosText.getText, dct = eidosText.getDctOpt, id = eidosText.getIdOpt)

    eidosText.getTitleOpt.foreach { titleOpt =>
      TitleDocumentAttachment.setTitle(annotatedDocument.document, titleOpt)
    }
    eidosText.getLocationOpt.foreach { location =>
      LocationDocumentAttachment.setLocation(annotatedDocument.document, location)
    }

    val corpus = new JLDCorpus(Seq(annotatedDocument))
    val jValue = corpus.serialize(adjectiveGrounder)

    jValue
  }

  if (prime)
    prepare


}
