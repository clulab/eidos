package org.clulab.wm.wmexchanger2.apps

import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser
import org.clulab.wm.wmexchanger2.utils.Environment
import org.clulab.wm.wmexchanger2.wmconsumer.RealRestOntologyConsumer

object DownloadOntology extends App {
  val username = "eidos"
  val password = args.lift(0).getOrElse(throw new RuntimeException("Password must be provided in args(0)."))
  val ontologyId = "8bb77947-6733-4428-add9-9badee7581ad"
  val service = "https://wm-ingest-pipeline-rest-1.prod.dart.worldmodelers.com/dart/api/v1/ontologies"

  Environment.setEnv {
    new java.util.HashMap[String, String]() {
    }
  }

  val realRestOntologyConsumer = new RealRestOntologyConsumer(service, username, password)
  realRestOntologyConsumer.open()

  val ontology = realRestOntologyConsumer.autoClose { realRestOntologyConsumer =>
    realRestOntologyConsumer.download(ontologyId)
  }
  println(ontology)
}
