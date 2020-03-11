package ai.lum.eidos.restapp.models

import ai.lum.eidos.restapp.EidosActor
import ai.lum.eidos.restapp.models.text.EidosText
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.event.LoggingReceive
import play.api.libs.json.JsValue

object Page {
  case class Read(eidosText: EidosText)
  case class Cancel()
  case class Filed()
}

class Page(library: ActorRef, reader: ActorRef) extends EidosActor {
  // It will send catalog updates of the transcript
  // Catalog will pick off the top item as the result
  protected var transcript: List[String] = List.empty

  override def receive: Receive = started

  def started: Receive = LoggingReceive {
    case Page.Read(eidosText) =>
      // Do I know which this goes to?  How can it be cancelled?
      reader ! Reader.Read(eidosText, context.self)
      this.context.become(reading(eidosText.getDocumentIdOpt.get))
//    case Page.Renew(documentId)
//    case Page.Delete(documentId)
    case Page.Cancel =>
      // There is nothing to cancel now
  }

  def reading(jobId: String): Receive = LoggingReceive {
    case Reader.Complete(jsValueEither: Either[Throwable, JsValue]) =>
      library ! Library.Create(jobId, jsValueEither)
      this.context.become(filing)
      // Add next thing to transcript and time, notify database?
      // Inform the database that I am complete by sending the message
      // It will kill me
    case Page.Cancel =>
      // Somehow cancel job 5
  }

  def filing: Receive = LoggingReceive {
    case Page.Filed => context.stop(self) // My job is done
  }
}
