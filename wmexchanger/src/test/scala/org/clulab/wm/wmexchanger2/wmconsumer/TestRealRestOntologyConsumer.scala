package org.clulab.wm.wmexchanger2.wmconsumer

class TestRealRestOntologyConsumer {
  val service = "/dart/api/v1/cdrs"
  val username = "eidos"
  val password = "quick_OHIO_flat_94"
  val realRestOntologyConsumer = new RealRestOntologyConsumer(service, username, password)

}
