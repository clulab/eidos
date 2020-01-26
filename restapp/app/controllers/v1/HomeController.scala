package controllers.v1

import javax.inject._
import play.api.mvc._
import play.api.mvc.Action

@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  def ping: Action[AnyContent] = Action {
    println("Ping function was called!")
    Ok
  }
}
