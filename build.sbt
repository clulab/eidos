import org.clulab.sbt.BuildUtils
import org.clulab.sbt.Resolvers

// See also the other files in the project directory with sbt extensions.
// They are generally named according to the task they are associated with:
// compile, initialize, publish, release, run, test, update, etc.
name := "eidos"
description := BuildUtils.singleLine("""
  |Eidos is an open-domain machine reading system designed by the Computational Language Understanding (CLU) Lab
  |at the University of Arizona for the World Modelers DARPA program.  It uses a cascade of Odin grammars to
  |extract events from free text.
""")

val scala11 = "2.11.12" // up to 2.11.12
val scala12 = "2.12.13" // up to 2.12.13
val scala13 = "2.13.5"  // up to 2.13.5

// Processors is not available for scala13, so it is skipped here.
ThisBuild / crossScalaVersions := Seq(scala12, scala11) // , scala13)
ThisBuild / scalaVersion := crossScalaVersions.value.head

resolvers ++= Seq(
  Resolvers.localResolver,  // Reserve for Two Six.
  Resolvers.clulabResolver, // glove
  Resolvers.jitpackResolver // Ontologies
)

libraryDependencies ++= {
  val playVersion = BuildUtils.sbtPluginVersion
  val  lihaoyiVer = "0.7.1"

  Seq(
    // These two are not needed for the docker file if the cache is used.
    "org.clulab"                % "geonames"                % "1.0.0+20200518T005330Z.gadmworedas",
    // Eidos now reuses the glove vectors from processors, so none of these is needed.
 // "org.clulab"               %% "glove-840b-300d-10f-bin" % "1.0.0", // abridged, binary, quick loading if compatible
 // "org.clulab"               %% "glove-840b-300d-10f"     % "1.0.0", // abridged, text, slower loading
 // "org.clulab"                % "glove-840b-300d"         % "0.1.0", // unabridged, text, slowest loading
    // The rest from org.clulab are always needed.
    "org.clulab"               %% "timenorm"                % "1.0.5" exclude("org.slf4j", "slf4j-log4j12"),
    "org.clulab"               %% "geonorm"                 % "1.0.0",
    // This is used for config utilities in particular.
    "ai.lum"                   %% "common"                  % "0.0.8",
    // This ontology is fetched from github rather than included directly.
    "com.github.WorldModelers"  % "Ontologies"              % "master-SNAPSHOT", // remote
//  "WorldModelers"            %% "ontologies"              % "0.3.0-SNAPSHOT",  // local
    // Web serialization needs this.
    "com.typesafe.play"        %% "play-json"               % playVersion,
    // This next one is used in MaaSUtils.
    "com.lihaoyi"              %% "upickle"                 % lihaoyiVer,
    // These are used for testing only.
    "com.github.jsonld-java"    % "jsonld-java"             % "0.12.0" % Test
  )
}

lazy val core = (project in file("."))
    .enablePlugins(BuildInfoPlugin)
    .disablePlugins(PlayScala, JavaAppPackaging, SbtNativePackager)
    .dependsOn(eidoscommon % "compile -> compile; test -> test", ontologies)
    .aggregate(eidoscommon, ontologies)
    .settings(
      assembly / aggregate := false
    )

lazy val eidoscommon = project

// Skip scala11 on this internal project.
lazy val elasticsearch = project
    .dependsOn(eidoscommon)

lazy val ontologies = project
    .dependsOn(eidoscommon)

lazy val webapp = project
    .enablePlugins(PlayScala)
    .dependsOn(core)

// Skip scala11 on this internal project.
lazy val wmexchanger = project
    .enablePlugins(JavaAppPackaging)
    .disablePlugins(PlayScala)
    .aggregate(core)
    .dependsOn(core % "compile -> compile; test -> test", eidoscommon)
