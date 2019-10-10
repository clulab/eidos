/* Code similar to this is automatically generated during project compilation. */

package org.clulab.wm.eidos.groundings

import java.time.ZonedDateTime

case class MockVersion(commit: String, date: ZonedDateTime)

object MockVersions {
  val codeDir = "src/main/resources/"
  val ontologyDir = codeDir + "org/clulab/wm/eidos/english/ontologies/"

  // This first value applies to the entire repository.
  val version: Option[MockVersion] = Some(MockVersion("2db42aa7c62d9b3b4cf99a08ec393121e53ce5cd", ZonedDateTime.parse("2019-10-07T17:00:49-07:00"))) // 2019-10-04T20:49:00Z")))

  // These values are for individual files.
  val versions: Map[String, Option[MockVersion]] = Map(
    ontologyDir + "un_ontology.yml" -> Some(MockVersion("8c3d191c837e973a9ebfacaa78d3a96ab1701981", ZonedDateTime.parse("2019-10-04T17:18:20Z"))),
    ontologyDir + "interventions.yml" -> Some(MockVersion("9fc7e0860cf54b7b54378bf3b73efe6e68e4e10b", ZonedDateTime.parse("2019-07-09T12:49:08Z")))
  )
}