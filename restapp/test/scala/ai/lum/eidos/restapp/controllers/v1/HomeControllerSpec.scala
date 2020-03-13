package ai.lum.eidos.restapp.controllers.v1

import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.test._
import play.api.test.Helpers._
import ai.lum.eidos.restapp.models.utils.QueryString

class HomeControllerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting {

  "HomeControllerV1" should {

    "respond to a ping" in {
      val request = FakeRequest(GET, "/eidos/v1/ping")
      val result = route(app, request).get

      status(result) mustBe OK
    }

    "respond to an echo" in {
      val text = "This is a test"
      val path = "/eidos/v1/echo"
      val query = QueryString.escape(Map("text" -> text))
      val uri = path + query
      val request = FakeRequest(POST, uri)
      val result = route(app, request).get

      contentAsString(result) must be (text)
    }
  }
}
