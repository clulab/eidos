package ai.lum.eidos.restapp.controllers.v3

import javax.inject._

import ai.lum.eidos.restapp.models.EidosStatus
import ai.lum.eidos.restapp.models.host.SerialHost
import ai.lum.eidos.restapp.models.text.CdrText
import ai.lum.eidos.restapp.models.utils.JsonUtils

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

import scala.collection.mutable

@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {
  import HomeController.logger

  val eidosHost = new SerialHost(false) // Should be true for production
  val eidosStatus = new EidosStatus

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
