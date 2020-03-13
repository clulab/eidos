package ai.lum.eidos.restapp.controllers.v1

import javax.inject._

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import play.api.mvc._

@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {
  import HomeController.logger

  def ping: Action[AnyContent] = Action {
    logger.info("Called 'ping' function!")
    Ok
  }

  def echo(text: String): Action[AnyContent] = Action {
    logger.info("Called 'echo' function!")
    Ok(text)
  }
}

object HomeController {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)
}
