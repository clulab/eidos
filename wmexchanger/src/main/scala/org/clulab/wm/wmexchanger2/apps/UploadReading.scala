package org.clulab.wm.wmexchanger2.apps

import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.wmexchanger2.wmproducer.RealRestProducer

import java.io.File

object UploadReading extends App {
  val username = "eidos"
  val password = args.lift(0).getOrElse(throw new RuntimeException("Password must be provided in args(0)."))
  val documentId = "0e20c0e1d1a7a3124fc0f31513234395"
  val ontologyId = "8bb77947-6733-4428-add9-9badee7581ad"
  val file = new File(s"../corpora/feb2022exp/output/done/$documentId.jsonld")
  val service = "https://wm-ingest-pipeline-rest-1.prod.dart.worldmodelers.com/dart/api/v1/readers/upload"
  val eidosVersion = "Feb2022IntTest"
  val ontologyVersion = "4.0"

  val realRestProducer = new RealRestProducer(service, username, password, eidosVersion, ontologyVersion)
  realRestProducer.open()

  val response = realRestProducer.autoClose { realRestProducer =>
    realRestProducer.upload(file, documentId, ontologyId)
  }
  println(response)
}
