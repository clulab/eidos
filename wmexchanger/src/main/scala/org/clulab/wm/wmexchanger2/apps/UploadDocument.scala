package org.clulab.wm.wmexchanger2.apps

import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.wmexchanger2.utils.Environment
import org.clulab.wm.wmexchanger2.wmconsumer.RealRestDocumentConsumer
import org.clulab.wm.wmexchanger2.wmproducer.RealRestProducer

import java.io.File

object UploadDocument extends App {
  val username = "eidos"
  val password = args.lift(0).getOrElse(throw new RuntimeException("Password must be provided in args(0)."))
  val file = new File("../corpora/feb2022exp/output/done/0e20c0e1d1a7a3124fc0f31513234395.jsonld")
  val service = "https://wm-ingest-pipeline-rest-1.prod.dart.worldmodelers.com/dart/api/v1/cdrs"
  val eidosVersion = "Feb2022IntTest"
  val ontologyVersion = "4.0"

  Environment.setEnv {
    new java.util.HashMap[String, String]() {
    }
  }

  val realRestProducer = new RealRestProducer(service, username, password, eidosVersion, ontologyVersion)
  realRestProducer.open()

  val document = realRestProducer.autoClose { realRestProducer =>
    realRestProducer.upload(file)
  }
  println(document)
}
