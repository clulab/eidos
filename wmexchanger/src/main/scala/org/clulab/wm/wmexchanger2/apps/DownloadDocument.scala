package org.clulab.wm.wmexchanger2.apps

import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.wmexchanger2.utils.Environment
import org.clulab.wm.wmexchanger2.wmconsumer.RealRestDocumentConsumer
import org.clulab.wm.wmexchanger2.wmconsumer.RealRestOntologyConsumer

object DownloadDocument extends App {
  val username = "eidos"
  val password = args.lift(0).getOrElse(throw new RuntimeException("Password must be provided in args(0)."))
  val documentId = "00a64b7eac4c87db4d575cf987612456"
  val service = "https://wm-ingest-pipeline-rest-1.prod.dart.worldmodelers.com/dart/api/v1/cdrs"

  Environment.setEnv {
    new java.util.HashMap[String, String]() {
    }
  }

  val realRestDocumentConsumer = new RealRestDocumentConsumer(service, username, password)
  realRestDocumentConsumer.open()

  val document = realRestDocumentConsumer.autoClose { realRestDocumentConsumer =>
    realRestDocumentConsumer.download(documentId)
  }
  println(document)
}
