import org.clulab.sbt.BuildUtils

ThisBuild / developers := List(
  Developer(
    id    = "mihai.surdeanu",
    name  = "Mihai Surdeanu",
    email = "mihai@surdeanu.info",
    url   = url("https://www.cs.arizona.edu/person/mihai-surdeanu")
  )
)
ThisBuild / homepage := Some(url("https://github.com/clulab/eidos"))
ThisBuild / licenses := List(
  "Apache License, Version 2.0" ->
  url("http://www.apache.org/licenses/LICENSE-2.0.html")
)
ThisBuild / organization := "org.clulab"
ThisBuild / organizationHomepage := Some(url("http://clulab.org/"))
ThisBuild / organizationName := "Computational Language Understanding (CLU) Lab"
// The sonatype plugin seems to overwrite these two values.
ThisBuild / pomIncludeRepository := BuildUtils.keepHttpRepos
ThisBuild / publishMavenStyle := true
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
    "scm:git@github.com:clulab/eidos.git"
  )
)
