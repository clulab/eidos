package controllers.v2

import ai.lum.eidos.host.SerialHost
import ai.lum.eidos.text.PlainText
import ai.lum.eidos.EidosStatus
import javax.inject._
import models.ai.lum.eidos.EidosException
import org.clulab.serialization.{json => Json}
import play.api.libs.json.{Json => JSon}
import org.json4s.JObject
import org.json4s.JField
import org.json4s.JString
import play.api.http.Status.SERVICE_UNAVAILABLE
import play.api.mvc.Action
import play.api.mvc._

import scala.util.Try

@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {
  val eidosHost = new SerialHost(false) // Should be true for production
  val eidosStatus = new EidosStatus

  def status: Action[AnyContent] = Action {
    println("Status function was called!")
    Ok(s"The status is ${eidosStatus.get}.")
  }

  // Make sure incoming is also utf-8
  def readText(text: String, titleOpt: Option[String], idOpt: Option[String], dateOpt: Option[String], locationOpt: Option[String]): Action[AnyContent] = Action {
    println("ReadText function was called!")
    if (eidosStatus.start)
      ServiceUnavailable("I was already busy")
    else {
      try {
        val eidosText = new PlainText(text, titleOpt, idOpt, dateOpt, locationOpt) // Need to catch exceptions
        //    val jValue = eidosHost.process(eidosText)
        //    val json = stringify(jValue, pretty = true)
        //    val jObject = JObject(JField("result", JString(text))) // This uses the scala4j version
        //    val json = Json.stringify(jObject, pretty = false)
        //    val jsValue = JSon.parse(json)

        val jObject = eidosText.toJson
        val json = Json.stringify(jObject, pretty = false)
        val jsValue = JSon.parse(json)

        Ok(jsValue)
      }
      catch {
        case exception: EidosException =>
          BadRequest(exception.getMessage)
      }
      finally {
        eidosStatus.stop
      }
    }
  }

  def readCdr(): Action[AnyContent] = Action {
    println("ReadCdr function was called!")
    if (eidosStatus.start)
      ServiceUnavailable("I was already busy")
    else {
      try {
        //      throw new EidosException("Too many questions", null)
        Thread.sleep(10000)
        Ok("Keith was here")
      }
      catch {
        case exception: EidosException =>
          ServiceUnavailable(exception.getMessage)
      }
      finally {
        eidosStatus.stop
      }
    }
  }
}
