package ai.lum.eidos.restapp.models

import java.time.LocalDateTime

class Provenance(val message: String) {
  val timestamp = LocalDateTime.now()
  // Include actor ref of who?
}

class AcceptedProvenance extends Provenance("Read request accepted")
