import ReleaseTransformations._

name := "eidos"
organization := "org.clulab"

scalaVersion := "2.12.4"
crossScalaVersions := Seq("2.11.11", "2.12.4")

//EclipseKeys.withSource := true

libraryDependencies ++= {
  val procVer = "7.3.1"

  Seq(
    "org.clulab"    %% "processors-main"          % procVer,
    "org.clulab"    %% "processors-corenlp"       % procVer,
    "org.clulab"    %% "processors-odin"          % procVer,
    "org.clulab"    %% "processors-modelsmain"    % procVer,
    "org.clulab"    %% "processors-modelscorenlp" % procVer,
    "ai.lum"        %% "common"                   % "0.0.8",
    "org.scalatest" %% "scalatest"                % "3.0.4" % "test",
    "commons-io"    %  "commons-io"               % "2.5",
    "com.typesafe"  %  "config"                   % "1.3.1",
    "net.sf.saxon"  % "saxon-dom"                 % "8.7",
    "org.slf4j"     % "slf4j-api"                 % "1.7.10",
    "com.github.jsonld-java"     % "jsonld-java"    % "0.12.0",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"
  )
}

//
// publishing settings
//
// publish to a maven repo
publishMavenStyle := true

// the standard maven repository
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

// let’s remove any repositories for optional dependencies in our artifact
pomIncludeRepository := { _ => false }

// mandatory stuff to add to the pom for publishing
pomExtra :=
  <url>https://github.com/clulab/eidos</url>
  <licenses>
    <license>
      <name>Apache License, Version 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/clulab/eidos</url>
    <connection>https://github.com/clulab/eidos</connection>
  </scm>
  <developers>
    <developer>
      <id>mihai.surdeanu</id>
      <name>Mihai Surdeanu</name>
      <email>mihai@surdeanu.info</email>
    </developer>
  </developers>
//
// end publishing settings
//

lazy val core = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoPackage := "org.clulab.wm.eidos",
    buildInfoOptions += BuildInfoOption.BuildTime,
    buildInfoKeys := Seq[BuildInfoKey](
      name, version, scalaVersion, sbtVersion, libraryDependencies, scalacOptions,
      "gitCurrentBranch" -> { git.gitCurrentBranch.value },
      "gitHeadCommit" -> { git.gitHeadCommit.value.getOrElse("") },
      "gitHeadCommitDate" -> { git.gitHeadCommitDate.value.getOrElse("") },
      "gitUncommittedChanges" -> { git.gitUncommittedChanges.value }
    )
  )

lazy val webapp = project
  .enablePlugins(PlayScala)
  .aggregate(core)
  .dependsOn(core)


test in assembly := {}
assemblyMergeStrategy in assembly := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case x => MergeStrategy.first
}

// release steps
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommandAndRemaining("+publishSigned"),
  setNextVersion,
  commitNextVersion,
  releaseStepCommandAndRemaining("sonatypeReleaseAll"),
  pushChanges
)

// scaladoc hosting
enablePlugins(SiteScaladocPlugin)
enablePlugins(GhpagesPlugin)
git.remoteRepo := "git@github.com:clulab/eidos.git"
