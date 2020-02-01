package ai.lum.eidos.restapp.controllers.v3

import javax.inject._
import ai.lum.eidos.restapp.models.EidosStatus
import ai.lum.eidos.restapp.models.host.SerialHost
import ai.lum.eidos.restapp.models.text.CdrText
import ai.lum.eidos.restapp.models.utils.JsonUtils
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor._
import akka.util.Timeout
import org.clulab.serialization.{json => Json}
import org.json4s.JField
import org.json4s.JObject
import org.json4s.JString
import org.json4s.MappingException
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import play.api.libs.json.{Json => JSon}
import play.api.libs.json.JsValue
import play.api.mvc._
import play.libs.concurrent.CustomExecutionContext

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

trait MyExecutionContext extends ExecutionContext

class MyExecutionContextImpl @Inject() (system: ActorSystem)
    extends CustomExecutionContext(system, "my.executor")
        with MyExecutionContext

object HelloActor {
  def props = Props[HelloActor]

  case class SayHello(name: String)
}

class HelloActor extends Actor {

  def receive = {
    case HelloActor.SayHello(name: String) =>
      sender() ! "Hello, " + name
  }
}

@Singleton
class HomeController @Inject()(system: ActorSystem, myExecutionContext: MyExecutionContext, cc: ControllerComponents) extends AbstractController(cc) {

  import scala.concurrent.duration._
  import akka.pattern.ask
  implicit val timeout: Timeout = 5.seconds
  val helloActor = system.actorOf(HelloActor.props, "hello-actor")

  def sayHello(name: String): Action[AnyContent] = Action.async {
    (helloActor ? HelloActor.SayHello("keith")).mapTo[String].map { message =>
      println(message)
    }(myExecutionContext)
  }


  import HomeController.logger

  val eidosHost = new SerialHost(false) // Should be true for production
  val eidosStatus = new EidosStatus

//  def intensiveComputation(): Future[Int] = Future[Int] {
//    Thread.sleep(10000)
//    10
//  }(myExecutionContext)

//  val result = intensiveComputation()
      // .withTimeout // This no longer exists!

//  def intensiveComputation2(): scala.actors.Future[Int] = scala.actors.Future {
//    Thread.sleep(10000)
//    10
//  }


  class DocumentState {

  }

  class Documents {
    val map: mutable.HashMap[String, DocumentState] = mutable.HashMap.empty
  }


  def status: Action[AnyContent] = Action {
    logger.info("Called 'status' function!")
    Ok(if (eidosStatus.get) "Busy" else "Ready")
  }

  def listDocuments(): Action[AnyContent] = Action {
    Ok // Do their statuses here so that know to download results
  }

  def newDocument(): Action[JsValue] = Action(parse.json) { request: Request[JsValue] =>
    logger.info("Called 'newDocument' function!")
    if (eidosStatus.start)
      ServiceUnavailable("I was already busy")
    else {
      try {
        val eidosText = new CdrText(JsonUtils.toJValue(request.body))
        val jValue = eidosText.toJson
        val jsValue = JsonUtils.toJsValue(jValue)

        Ok(jsValue)
      }
      catch {
        case exception: MappingException =>
          BadRequest(exception.getMessage)
      }
      finally {
        eidosStatus.stop
      }
    }
  }

  def getDocument(documentId: String) = Action {
    Ok // Return parse of some string or another
  }

  def deleteDocument(documentId: String) = Action {
    Ok // Return parse of some string or another
  }

  def cancelDocument(documentId: String) = Action {
    Ok // Return parse of some string or another
  }

  def statusDocument(documentId: String) = Action {
    Ok // Return parse of some string or another
  }
}

object HomeController {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)
}
