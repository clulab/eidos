package ai.lum.eidos.restapp

import akka.actor.Actor
import akka.actor.ActorLogging

trait EidosActor extends Actor with ActorLogging {
  // These logging functions come from processors.
  // Why do preStart and postStop not call super?

  // lifecycle methods for tracing
//  override def preStart (): Unit =
//    log.debug(s"Actor ${self} starting...")
//
//  override def preRestart (reason: Throwable, msg: Option[Any]): Unit = {
//    log.debug(s"Actor ${self} restarting...")
//    super.preRestart(reason, msg)
//  }
//
//  override def postRestart (reason: Throwable): Unit = {
//    log.debug(s"Actor ${self} ...restarted")
//    super.postRestart(reason)
//  }
//
//  override def postStop (): Unit =
//    log.debug(s"Actor ${self} stopped")
}
