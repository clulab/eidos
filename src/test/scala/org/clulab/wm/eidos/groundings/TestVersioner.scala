package org.clulab.wm.eidos.groundings

// Switch these back and forth to test code generation
//import org.clulab.wm.eidos.groundings.{MockVersions => TestVersions, MockVersion => TestVersion }
import com.github.clulab.eidos.{Versions => TestVersions, Version => TestVersion }
import java.time.ZonedDateTime

import org.scalatest._

class TestVersioner extends FlatSpec with Matchers {
  val now = ZonedDateTime.now

  behavior of "Versions"

  def test(file: String, version: Option[TestVersion], expirationDate: ZonedDateTime): Unit = {
    it should "should document version of " + file in {

      version.nonEmpty should be (true)
      version.get.commit.nonEmpty should be (true)
      version.get.date.isBefore(expirationDate) should be (true)
      println(file + ": " + version)

      if (file.contains('/'))
        file.startsWith(MockVersions.ontologyDir) should be (true)
    }
  }

  test("HEAD", TestVersions.version, now)

  TestVersions.versions.nonEmpty should be (true)

  TestVersions.versions.foreach { case (file, version) =>
    test(file, version, TestVersions.version.map { _.date }.getOrElse(now))
  }
}
