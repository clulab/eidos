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
ThisBuild / organization := "org.clulab"
ThisBuild / organizationHomepage := Some(url("http://clulab.cs.arizona.edu/"))
ThisBuild / organizationName := "CLU Lab"
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/" // the standard maven repository
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/clulab/eidos"),
    "scm:git:https://github.com/clulab/eidos.git"
  )
)
