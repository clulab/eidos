package org.clulab.wm.eidos.apps.batch

import org.clulab.serialization.json.stringify
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.apps.batch.ExtractDartMetaFromDirectory.outputDir
import org.clulab.wm.eidos.apps.batch.utils.Counter
import org.clulab.wm.eidos.apps.batch.utils.TsvUtils
import org.clulab.wm.eidos.groundings.EidosAdjectiveGrounder
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.serialization.json.JLDDeserializer
import org.clulab.wm.eidos.serialization.json.JLDSerializer
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.clulab.wm.eidos.utils.FileUtils
import org.clulab.wm.eidos.utils.Sinker
import org.clulab.wm.eidos.utils.StringUtils
import org.clulab.wm.eidos.utils.meta.DartZipMetaUtils

object ReconstituteAndRegroundEstonia extends App {
  val inputDir = args(0)
  val outputDir = args(1)

  val files = FileUtils.findFiles(inputDir, "jsonld")
  val config = EidosSystem.defaultConfig
  val eidosSystem = new EidosSystem(config)
  val adjectiveGrounder = EidosAdjectiveGrounder.fromEidosConfig(config)
  val serializer = new JLDSerializer(Some(adjectiveGrounder))
  val deserializer = new JLDDeserializer()

  files.foreach { file =>
    val json = FileUtils.getTextFromFile(file)
    val oldCorpus = deserializer.deserialize(json)
    val oldAnnotatedDocument = oldCorpus.head
    val newAnnotatedDocument = eidosSystem.postProcess(oldAnnotatedDocument)
    val corpus = new JLDCorpus(Seq(newAnnotatedDocument))
    val mentionsJSONLD = corpus.serialize(adjectiveGrounder)
    val path = outputDir + "/" + file.getName

    FileUtils.printWriterFromFile(path).autoClose { pw =>
      pw.println(stringify(mentionsJSONLD, pretty = true))
    }
  }
}
