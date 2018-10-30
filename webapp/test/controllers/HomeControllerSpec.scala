package controllers

import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.test._
import play.api.test.Helpers._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 *
 * For more information, see https://www.playframework.com/documentation/latest/ScalaTestingWithScalaTest
 */
class HomeControllerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting {

  "HomeController GET" should {

    "render the index page from a new instance of controller" in {
      val controller = new HomeController(stubControllerComponents())
      val home = controller.index().apply(FakeRequest(GET, "/"))

      status(home) mustBe OK
      contentType(home) mustBe Some("text/html")
      contentAsString(home) must include ("World Modelers Visualizer")
    }

    "render the index page from the application" in {
      val controller = inject[HomeController]
      val home = controller.index().apply(FakeRequest(GET, "/"))

      status(home) mustBe OK
      contentType(home) mustBe Some("text/html")
      contentAsString(home) must include ("World Modelers Visualizer")
    }

    "render the index page from the router" in {
      val request = FakeRequest(GET, "/")
      val home = route(app, request).get

      status(home) mustBe OK
      contentType(home) mustBe Some("text/html")
      contentAsString(home) must include ("World Modelers Visualizer")
    }
  }

  "HomeController POST" should {
    "accept request with text parameter and return JSON" in {

      // Note that the request fails because the JSON does not have key 'text' but instead has key 'text123'
      // This is because testing an actual run requires initialization which takes too long

      val testJson = Json.parse("""{ "text123": "Drought causes regional instability." }""")
      val request = FakeRequest(POST, "/process_text").withJsonBody(testJson)
      val result = route(app, request).get

      contentAsString(result) must include ("Missing parameter [text]")
    }
  }
  
}
