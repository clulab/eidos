package org.clulab.wm.eidos.groundings

// Switch these back and forth to test code generation
//import org.clulab.wm.eidos.groundings.{MockVersions => TestVersions, MockVersion => TestVersion }
import java.time.ZonedDateTime
import com.github.clulab.eidos.{Version => TestVersion, Versions => TestVersions}
import org.clulab.wm.eidos.test.Test

class TestVersioner extends Test {
  val now = ZonedDateTime.now

  behavior of "Versions"

  def test(file: String, version: Option[TestVersion], expirationDate: ZonedDateTime): Unit = {
    it should "should document version of " + file in {

      version.nonEmpty should be (true)
      version.get.commit.nonEmpty should be (true)

      val versionDate = version.get.date

      (versionDate.isBefore(expirationDate) || versionDate.isEqual(expirationDate)) should be (true)
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
