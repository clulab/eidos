package ai.lum.eidos.restapp.models

import ai.lum.eidos.restapp.EidosActor
import ai.lum.eidos.restapp.models.host.IlliterateHost
import ai.lum.eidos.restapp.models.host.SerialHost
import ai.lum.eidos.restapp.models.text.EidosText
import ai.lum.eidos.restapp.models.utils.JsonUtils
import akka.actor.ActorRef
import akka.event.LoggingReceive
import play.api.libs.json.JsValue

object Reader {
  case class Read(eidosText: EidosText, job: ActorRef)
  case class Complete(jsValueEither: Either[Throwable, JsValue])

  // Who should make this?
  val eidosHost = {
    println("Starting to make the host")
    val host = new IlliterateHost(prime = true)
    println("Finished making the host")
    host
  }
}

// Make enough of these readers to fill up the number of processors
// TODO This needs to be something that can be cancelled.  How?
class Reader extends EidosActor {

  override def receive: Receive = LoggingReceive {
    case Reader.Read(eidosText, job) =>
      try {
        // This blocks.
        val jValue = Reader.eidosHost.process(eidosText)
        val jsValue = JsonUtils.toJsValue(jValue)

        job ! Reader.Complete(Right(jsValue))
      }
      catch {
        case throwable: Throwable =>
          log.error(throwable, "Eidos threw an exception.")
          val eidosException = new EidosException("Eidos threw an exception", throwable)
          job ! Reader.Complete(Left(eidosException))
      }
  }
}
