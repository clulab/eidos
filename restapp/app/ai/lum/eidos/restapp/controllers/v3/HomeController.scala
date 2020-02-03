package ai.lum.eidos.restapp.controllers.v3

import ai.lum.eidos.restapp.models.Reader
import javax.inject._
import ai.lum.eidos.restapp.models.EidosStatus
import ai.lum.eidos.restapp.models.Library
import ai.lum.eidos.restapp.models.Page
import ai.lum.eidos.restapp.models.host.SerialHost
import ai.lum.eidos.restapp.models.text.CdrText
import ai.lum.eidos.restapp.models.text.PlainText
import ai.lum.eidos.restapp.models.utils.JsonUtils
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.SupervisorStrategy.Restart
import akka.actor._
import akka.routing.FromConfig
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

object HomeController {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)
}

// These can all be made more complicated.
// They can even access systems running elsewhere, in other JVMs.
class LibraryProvider(actorSystem: ActorSystem) {
  val props: Props = Props(classOf[Library])

  def newInstance(): ActorRef = {
    actorSystem.actorOf(props, "library")
  }
}

class ReaderProvider(actorSystem: ActorSystem) {
  val props: Props = Props(classOf[Reader])

  def newSingleInstance(): ActorRef = {
    actorSystem.actorOf(props, "reader")
  }

  // TODO: Could decide here which kind of Reader is desired.
  def newMultipleInstance(): ActorRef = {
    val restartEachStrategy: SupervisorStrategy =
        OneForOneStrategy() { case _ => Restart }
    val routedProps = props
        .withRouter(FromConfig.withSupervisorStrategy(restartEachStrategy))

    actorSystem.actorOf(routedProps, "readers")
  }

  def newInstance(): ActorRef = {
//    newSingleInstance()
    newMultipleInstance()
  }
}

// These could be provided by something else, a pool?
// As many of these are made as are necessary to run the jobs.
// Should the job (somejobs) have a timeout in which the page is cancelled
// and the response is too busy?
class PageProvider(actorSystem: ActorSystem, library: ActorRef, reader: ActorRef) {
  val props: Props = Props(classOf[Page], library, reader)

  def newInstance(): ActorRef = {
    actorSystem.actorOf(props) // These each need a unique name.
  }
}

@Singleton
class HomeController @Inject()(actorSystem: ActorSystem, executionContext: ExecutionContext,
    controllerComponents: ControllerComponents) extends AbstractController(controllerComponents) {
  import HomeController.logger


  // This is most like the ProcessorServer
  // It makes the instance of Eidos, and then creates enough Readers to use it?
  // Instead, I am having the reader object create the instance
  // Is that wrong?  Would like to do all stuff in main

  val libraryProvider = new LibraryProvider(actorSystem)
  // There should only be one library per controller, because some
  // requests for information will be directed to this library.
  val library = libraryProvider.newInstance

  val readerProvider = new ReaderProvider(actorSystem)
  val reader = readerProvider.newInstance // Here is where the single reader is created
  // This is the one the pager will use.  Can this come from a pool instead?
  // Should be a pool of readers, which will all use the same underlying host

  val pageProvider = new PageProvider(actorSystem, library, reader)

//  def status: Action[AnyContent] = Action {
//     This may be some kind of internal thing only.
//    logger.info("Called 'status' function!")
//    Ok
//  }

  def listDocuments(): Action[AnyContent] = Action {
    // Send something to library and wait for response?
    Ok // Do their statuses here so that know to download results
  }

  def newDocument(): Action[JsValue] = Action(parse.json) { request: Request[JsValue] =>
    Ok
  }

//  def newDocument(): Action[JsValue] = Action(parse.json) { request: Request[JsValue] =>
  def status: Action[AnyContent] = Action {
    // Check with the library and see if document id is already there.
    // If it is, renew it instead of reprocess.
    // DocumentID needs to be required!
    try {
      //val eidosText = new CdrText(JsonUtils.toJValue(request.body))
      val eidosText = new PlainText("This is a test")
      0.until(6).foreach { index =>
        val page = pageProvider.newInstance

        page ! Page.Read(eidosText)
      }
      Ok
    }
    catch {
      case exception: MappingException => BadRequest(exception.getMessage)
    }
  }

  def getDocument(documentId: String) = Action {
    // Gets the actual data

    // Send this to the library, it may be that it isn't finished
    Ok // Return parse of some string or another
  }

  def deleteDocument(documentId: String) = Action {
    // Send this to the library
    Ok // Return parse of some string or another
  }

  def cancelDocument(documentId: String) = Action {
    // Send this to the approprate pager
    Ok // Return parse of some string or another
  }

  def statusDocument(documentId: String) = Action {
    // Send this to the library
    Ok // Return parse of some string or another
  }
}

/*
  val system = ActorSystem("procServer", config)

  logger.debug(s"(ProcessorServer.ctor): system=${system}")

  // create supervisory strategy for the router to handle errors
  private final val restartEachStrategy: SupervisorStrategy =
    OneForOneStrategy() { case _ => Restart }

  // create a router to a pool of processor actors waiting for work
  // this is the pool of processors, should give this pool to the pages?

  private val procPool: ActorRef = system.actorOf(
    ProcessorActor.props(processor).withRouter(
      FromConfig.withSupervisorStrategy(restartEachStrategy)),
    "procActorPool")

  logger.debug(s"(ProcessorServer.ctor): procPool=${procPool}")

  /** A process to finally stop the server after the router dies. */
  private val serverDeathWatcher = system.actorOf(
    ServerDeathWatchActor.props(system, procPool), "serverDeathWatcher")

  /** Return the reference to the internal instance of the pooled router. */
  def router: ActorRef = procPool

  /** Terminate the server: stop all children followed by the guardian actor. */
  def terminate: Unit = system.terminate()
 */

/*
  /** Shutdown this client: terminate the actor system. */
  override def shutdownClient: Unit = system.terminate()

  /** Shutdown the remote processor server AND this client. */
  override def shutdownClientServer: Unit = {
    this.shutdownServer
    this.shutdownClient
  }

  /** Send the server a message to shutdown actors and terminate the server router. */
  override def shutdownServer: Unit = {
    router ! Broadcast(PoisonPill)
    router ! PoisonPill
  }

 */

/*
  /** Send the given message to the server and block until response comes back. */
  private def callServer (request: ProcessorCSCommand): ProcessorCSReply = {
    val response = router ? request         // call returns Future within long timeout
    val result = Await.result(response, Duration.Inf) // blocking: wait forever
    if (result.isInstanceOf[ServerExceptionMsg]) {
      val exception = result.asInstanceOf[ServerExceptionMsg].exception
      logger.error(exception.getMessage)
      shutdownClient                        // shutdown the client system
      throw new RuntimeException(exception)
    }
    else
      result.asInstanceOf[ProcessorCSReply]
  }
 */