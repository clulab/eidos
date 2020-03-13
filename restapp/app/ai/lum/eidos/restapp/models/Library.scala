package ai.lum.eidos.restapp.models

import java.time.LocalDateTime

import ai.lum.eidos.restapp.EidosActor
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.event.LoggingReceive
import play.api.libs.json.JsValue

import scala.collection.mutable


// provenance

object Library {
  case class Create(documentId: String, reading: Either[Throwable, JsValue])
  case class Read(documentId: String)
  case class Update(documentId: String, provenance: List[String]) // Don't know if necessary
  case class Delete(documentId: String)

  case class Expire(documentId: String) // remove results from memory
  // This is being put into a command to ensure thread safety?
  // Expire removes result, especially large one

  case class Status(documentId: String) // Who wants to know?, reads the metadata

  case class DirectoryRequest()
  case class DirectoryResponse(documentIds: List[String])
}

class LibraryCard {
  var provenance: Provenance = null
  var readingResult = new ReceivedReadingResult
  // Library sets expiration when it receives the entry?
  var expirationOpt: Option[LocalDateTime] = None
}

class Library extends EidosActor {
  // It will send catalog updates of the transcript
  // Catalog will pick off the top item as the result
  protected val map: mutable.Map[String, List[String]] = mutable.Map.empty // Have to make this mutable

  override def receive: Receive = LoggingReceive {
    case Library.Create(documentId, reading) =>
      map(documentId) = List(reading.toString) // Should this be a future, nice that blocks access to map though
      sender ! Page.Filed
    case Library.Read(documentId: String) =>

    case Library.Update(documentId, provenance) =>

    case Library.Update(documentId: String, provenance: List[String]) =>

    case Library.Delete(documentId) =>

    case Library.DirectoryRequest() =>
      val documentIds: List[String] = map.keys.toList.sorted
      sender ! Library.DirectoryResponse(documentIds)
  }
}
