import org.clulab.sbt.BuildUtils

// See also the other files in the project directory with sbt extensions.
// They are generally named according to the task they are associated with:
// compile, initialize, publish, release, run, test, update, etc.
name := "eidos"
description := BuildUtils.singleLine("""
  |Eidos is an open-domain machine reading system designed by the Computational Language Understanding (CLU) Lab
  |at the University of Arizona for the World Modelers DARPA program.  It uses a cascade of Odin grammars to
  |extract events from free text.
""")

val scala11 = "2.11.11" // up to 2.11.12
val scala12 = "2.12.4"  // up to 2.12.13
val scala13 = "2.13.5"  // up to 2.13.5

ThisBuild / organization := "org.clulab"
ThisBuild / scalaVersion := scala12
ThisBuild / crossScalaVersions := Seq(scala11, scala12)

resolvers ++= Seq(
  "jitpack" at "https://jitpack.io", // needed by Ontologies
  // This is needed by processors-main, geonames, glove-840b-300d
  ("Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release")
      .withAllowInsecureProtocol(true)
)

// This doesn't work as ThisBuild, Zero, publish, or anything else.
val publishSettings = Seq(
  publishMavenStyle := true,
  pomIncludeRepository := BuildUtils.keepHttpRepos
)

libraryDependencies ++= {
  val sbtPluginVersion = BuildUtils.getProperty("./project/build.properties", "sbt-plugin.version")

  Seq(
    // These two are not needed for the docker file if the cache is used.
    "org.clulab"                % "geonames"         % "1.0.0+20200518T005330Z.gadmworedas",
    "org.clulab"                % "glove-840b-300d"  % "0.1.0",
    // The rest from org.clulab are always needed.
    "org.clulab"               %% "timenorm"         % "1.0.5" exclude("org.slf4j", "slf4j-log4j12"),
    "org.clulab"               %% "geonorm"          % "1.0.0",
    // This is used for config utilities in particular.
    "ai.lum"                   %% "common"           % "0.0.8",
    // This ontology is fetched from github rather than included directly.
    "com.github.WorldModelers"  % "Ontologies"       % "master-SNAPSHOT",
    // Web serialization needs this.
    "com.typesafe.play"        %% "play-json"        % sbtPluginVersion,
    // This next one is used in MaaSUtils.
    "com.lihaoyi"              %% "upickle"          % "0.7.1",
    // These are used for testing only.
    "org.scalatest"            %% "scalatest"        % "3.0.4"  % Test,
    "com.github.jsonld-java"    % "jsonld-java"      % "0.12.0" % Test
  )
}

lazy val core = (project in file("."))
    .enablePlugins(BuildInfoPlugin)
    .dependsOn(eidoscommon % "compile->compile;test->test", ontologies)
    .aggregate(eidoscommon, ontologies, webapp)
    .settings(publishSettings)

// This prevents "error: recursive lazy value core needs type".
lazy val coreRef = LocalProject("core")

lazy val eidoscommon = project
    .settings(publishSettings)

lazy val elasticsearch = project
    .dependsOn(eidoscommon)

lazy val ontologies = project
    .dependsOn(eidoscommon)
    .settings(publishSettings)

lazy val webapp = project
    .enablePlugins(PlayScala)
    .dependsOn(coreRef)
    .settings(publishSettings)

lazy val wmexchanger = project
    .dependsOn(eidoscommon)
