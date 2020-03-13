package org.clulab.wm.eidos.apps.batch

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder
import org.clulab.wm.eidos.mentions.EidosMention
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.serialization.json.JLDDeserializer
import org.clulab.wm.eidos.serialization.json.JLDSerializer
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileEditor
import org.clulab.wm.eidos.utils.FileUtils

object ReconstituteAndRegroundEstonia extends App {
  val inputDir = args(0)
  val outputDir = args(1)

  val files = FileUtils.findFiles(inputDir, "jsonld")
  val config = EidosSystem.defaultConfig
  val eidosSystem = new EidosSystem(config)
  val deserializer = new JLDDeserializer()
  val adjectiveGrounder = EidosAdjectiveGrounder.fromEidosConfig(config)
  val serializer = new JLDSerializer(Some(adjectiveGrounder))

  files.foreach { file =>
    val json = FileUtils.getTextFromFile(file)
    val oldCorpus = deserializer.deserialize(json)
    val oldEidosMentions = oldCorpus.head.eidosMentions
    val newEidosMentions = eidosSystem.components.ontologyHandler.ground(oldEidosMentions)
    val newAnnotatedDocument = AnnotatedDocument(oldCorpus.head.document, newEidosMentions)
    val corpus = new JLDCorpus(Seq(newAnnotatedDocument))
    val mentionsJSONLD = corpus.serialize(adjectiveGrounder)
    val path = FileEditor(file).setDir(outputDir).get

    FileUtils.printWriterFromFile(path).autoClose { pw =>
      pw.println(stringify(mentionsJSONLD, pretty = true))
    }
  }
}
