import org.clulab.sbt.BuildUtils

val publication = "eidos"

ThisBuild / developers := List(
  Developer(
    id    = "mihai.surdeanu",
    name  = "Mihai Surdeanu",
    email = "mihai@surdeanu.info",
    url   = url("https://www.cs.arizona.edu/person/mihai-surdeanu")
  )
)
ThisBuild / homepage := Some(url(s"https://github.com/clulab/$publication"))
ThisBuild / licenses := List(
  "Apache License, Version 2.0" ->
  url("http://www.apache.org/licenses/LICENSE-2.0.html")
)
ThisBuild / organization := "org.clulab"
ThisBuild / organizationHomepage := Some(url("http://clulab.org/"))
ThisBuild / organizationName := "Computational Language Understanding (CLU) Lab"
// The sonatype plugin seems to overwrite these two values, so they are in sonatype.sbt.
// ThisBuild / pomIncludeRepository := BuildUtils.keepHttpRepos
// ThisBuild / publishMavenStyle := true
ThisBuild / publishTo := {
  if (BuildUtils.useArtifactory) {
    val artifactory = "http://artifactory.cs.arizona.edu:8081/artifactory/"
    val repository = "sbt-release-local"
    val details =
        if (isSnapshot.value) ";build.timestamp=" + new java.util.Date().getTime
        else ""
    val location = artifactory + repository + details

    Some(("Artifactory Realm" at location).withAllowInsecureProtocol(true))
  }
  else {
    // This is for maven central, the default for when not artifactory.
    val nexus = "https://oss.sonatype.org/" // the standard maven repository
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
}
ThisBuild / scmInfo := Some(
  ScmInfo(
    url(s"https://github.com/clulab/$publication"),
    s"scm:git@github.com:clulab/$publication.git"
  )
)
