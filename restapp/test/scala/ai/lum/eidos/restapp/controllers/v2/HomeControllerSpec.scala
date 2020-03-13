package ai.lum.eidos.restapp.controllers.v2

import ai.lum.eidos.restapp.models.utils.QueryString

import org.clulab.wm.eidos.utils.Closer.AutoCloser

import org.scalatestplus.play._
import org.scalatestplus.play.guice._

import play.api.libs.json.{Json => JSon}
import play.api.test.Helpers._
import play.api.test._

import scala.io.Source

class HomeControllerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting {

  def format(text: String): String = "\"" + text + "\":"

  "HomeControllerV2" should {

    "respond to a status" in {
      val request = FakeRequest(GET, "/eidos/v2/status")
      val result = route(app, request).get

      status(result) mustBe OK
      contentType(result) mustBe Some("text/plain")
      contentAsString(result) must startWith("The status is ")
    }

    "respond to a readText" in {
      val text = "This is the text"
      val title = "This is the title"
      val id = "This is the id"
      val date = "2020-01-27"
      val location = "http://ai.lum.com"
      val path = "/eidos/v2/readText"
      val query = QueryString.escape(Map(
        // Comment some out to test different combinations.
        "text" -> text,
        "title" -> title,
        "id" -> id,
        "date" -> date,
        "location" -> location
      ))
      val uri = path + query
      val request = FakeRequest(GET, uri)
      val result = route(app, request).get
      val content = contentAsString(result)

      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      content must include(format("text"))
      content must include(format("title"))
      content must include(format("id"))
      content must include(format("dct"))
      content must include(format("location"))
    }

    "respond to a readCdr" in {
      val cdr = Source.fromResource("cdr.json").autoClose(_.mkString)
      val jsValue = JSon.parse(cdr)
      val path = "/eidos/v2/readCdr"
      val uri = path
      val request = FakeRequest(POST, uri).withJsonBody(jsValue)
      val result = route(app, request).get
      val content = contentAsString(result)

      status(result) mustBe OK
      contentType(result) mustBe Some("application/json")
      content must include(format("text"))
      content must include(format("title"))
      content must include(format("id"))
      content must include(format("dct"))
      content must include(format("location"))
    }
  }
}
