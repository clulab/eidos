package org.clulab.eidos.akkaapp.rest

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorSystem
import akka.actor.Props
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.Directives.concat
import akka.http.scaladsl.server.Directives.get
import akka.http.scaladsl.server.Directives.parameter
import akka.http.scaladsl.server.Directives.path
import akka.http.scaladsl.server.Directives.put
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.duration.SECONDS
import scala.concurrent.Future

object Rest {
  case class Start(actorSystem: ActorSystem, port: Int)
  case class Stop()
}

class Rest extends Actor with ActorLogging {

  val route: Route =
    path("hello") {
      concat(
        put {
          println("Put hello")
          complete((StatusCodes.Accepted, "Put hello"))
        },
        get {
          println("Get hello")
//          implicit val timeout: Timeout = 5.seconds
          complete((StatusCodes.Accepted, "Get hello"))
        }
      )
    }

  override def receive: Receive = stopped

  implicit val system = ActorSystem()

  def stopped: Receive = {
    case Rest.Start(actorSystem: ActorSystem, port) =>
      val materializer = ActorMaterializer()
      val bindingFuture: Future[Http.ServerBinding] = Http().bindAndHandle(route, "localhost", port)(materializer)
      context.become(started(bindingFuture))
      println(s"Server online at http://localhost:$port.")
    case _ => log.info("Invalid message in stopped state")
  }

  def starting(bindingFuture: Future[Http.ServerBinding]): Receive = {
    case _ => log.info("Invalid message in starting state")
  }

  def started(bindingFuture: Future[Http.ServerBinding]): Receive = {
    case Rest.Stop =>
      context.become(stopped)
      val result = bindingFuture.flatMap { binding =>
        println("I am about to unbind")
        val result = binding.unbind()
        println("I am now unbound." + result)
        println(result)
        result
      }(system.dispatcher)
      Await.ready(result, Duration(1, SECONDS))
      println("It finally got here!")
    case _ => log.info("Invalid message in started state")
  }

  def stopping: Receive = {
    case _ => log.info("Invalid message in started state")
      println(s"Server offline at http://localhost.")
  }
}
