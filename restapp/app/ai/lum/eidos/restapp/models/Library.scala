package ai.lum.eidos.restapp.models

import ai.lum.eidos.restapp.EidosActor
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.event.LoggingReceive


// provenance

object Library {
  case class Update(documentId: String, provenance: Seq[String])
  case class Expire(documentId: String)
  // This is being put into a command to ensure thread safety?
  case class Status(documentId: String) // Who wants to know?
  case class List() // Who wants to know?
}

class Library extends EidosActor {
  // It will send catalog updates of the transcript
  // Catalog will pick off the top item as the result
  protected val map: Map[String, List[String]] = Map.empty // Have to make this mutable

  override def receive: Receive = LoggingReceive {
    case Library.Update(documentId, provenance) =>

  }
}
