ThisBuild / developers := List(
  Developer(
    id    = "mihai.surdeanu",
    name  = "Mihai Surdeanu",
    email = "mihai@surdeanu.info",
    url   = url("http://surdeanu.info/mihai/")
  )
)
ThisBuild / homepage := Some(url("https://github.com/clulab/eidos"))
ThisBuild / licenses := List(
  "Apache License, Version 2.0" ->
  url("http://www.apache.org/licenses/LICENSE-2.0.html")
)
// Publish to a maven repo.
ThisBuild / publishMavenStyle := true
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/" // the standard maven repository
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
