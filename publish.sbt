import sbt.Keys.{developers, homepage, licenses, scmInfo}
import sbt.url

// publish to a maven repo
ThisBuild / publishMavenStyle := true

// the standard maven repository
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

// Let’s remove any repositories for optional dependencies in our artifact.
ThisBuild / pomIncludeRepository := { (repo: MavenRepository) =>
  repo.root.startsWith("http://artifactory.cs.arizona.edu")
}

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/clulab/eidos"),
    "scm:git:https://github.com/clulab/eidos.git"
  )
)

ThisBuild / licenses := List("Apache License, Version 2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.html"))

ThisBuild / homepage := Some(url("https://github.com/clulab/eidos"))

ThisBuild / developers := List(
  Developer(
    id    = "mihai.surdeanu",
    name  = "Mihai Surdeanu",
    email = "mihai@surdeanu.info",
    url   = url("http://surdeanu.info/mihai/")
  )
)
