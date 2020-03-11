package org.clulab.eidos.akkaapp.app

import akka.actor.ActorSystem
import akka.actor.Props
import akka.stream.ActorMaterializer
import org.clulab.eidos.akkaapp.rest.Rest

import scala.io.StdIn

object Main extends App {
  val actorSystem = ActorSystem()
  val executionContext = actorSystem.dispatcher

  // Make lots of other actors

  val rest = actorSystem.actorOf(Props[Rest], "rest")

  rest ! Rest.Start(actorSystem, 8080)
  println("Hit enter to stop the system")
  StdIn.readLine()
  rest ! Rest.Stop

  println("Press enter to start again")
  StdIn.readLine()
  rest ! Rest.Start(actorSystem, 8081)
  println("Hit enter to stop the system")
  StdIn.readLine()
  rest ! Rest.Stop

  println("I am exiting, but the program may not.")
//  rest ! kill
  actorSystem.terminate
}
