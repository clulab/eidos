package ai.lum.eidos.restapp.controllers.v2

import javax.inject._

import ai.lum.eidos.restapp.models.EidosException
import ai.lum.eidos.restapp.models.EidosStatus
import ai.lum.eidos.restapp.models.host.SerialHost
import ai.lum.eidos.restapp.models.text.CdrText
import ai.lum.eidos.restapp.models.text.PlainText
import ai.lum.eidos.restapp.models.utils.JsonUtils

import org.clulab.serialization.{json => Json}

import org.json4s.JField
import org.json4s.JObject
import org.json4s.JString
import org.json4s.MappingException
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import play.api.http.Status.SERVICE_UNAVAILABLE
import play.api.libs.json.{Json => JSon}
import play.api.libs.json.JsValue
import play.api.mvc._

import scala.util.Try

@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {
  import HomeController.logger

  val eidosHost = new SerialHost(false) // Should be true for production
  val eidosStatus = new EidosStatus

  def status: Action[AnyContent] = Action {
    logger.info("Called 'status' function!")
    Ok(s"The status is ${eidosStatus.get}.")
  }

  // Make sure incoming is also utf-8
  def readText(text: String, titleOpt: Option[String], idOpt: Option[String], dateOpt: Option[String], locationOpt: Option[String]): Action[AnyContent] = Action {
    logger.info("Called 'readText' function!")
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

        val jValue = eidosText.toJson
        val jsValue = JsonUtils.toJsValue(jValue)

//        Thread.sleep(10000)
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

  def readCdr(): Action[JsValue] = Action(parse.json) { request: Request[JsValue] =>
    logger.info("Called 'readCdr' function!")
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
}

object HomeController {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)
}
