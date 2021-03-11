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

// This is useful because timenorm loads a dll and only one dll is allowed per (Java) process.
// If it isn't here, sbt test (and run) can seemingly only be performed once before it will fail with
// java.lang.UnsatisfiedLinkError: no jnihdf5 in java.library.path
// The reasoning above needs to be reconsidered, because forking results in loss of menu
// functionality in things like EidosShell.  Output gets logged and, more importantly,
// input is blocked with null always being returned.
//fork := true

ThisBuild / organization := "org.clulab"
ThisBuild / scalaVersion := "2.12.4"
ThisBuild / crossScalaVersions := Seq("2.11.11", "2.12.4")

resolvers ++= Seq(
  "jitpack" at "https://jitpack.io", // needed by Versioner
  ("Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release").withAllowInsecureProtocol(true) // needed by processors-main, geonames, glove-840b-300d
)

libraryDependencies ++= {
  val sbtPluginVersion = BuildUtils.getProperty("./project/build.properties", "sbt-plugin.version")

  Seq(
    "org.clulab"               %% "timenorm"         % "1.0.5",
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
    "com.github.jsonld-java"    % "jsonld-java"      % "0.12.0" % Test,
    // These are not needed for the docker file if the cache is used.
    "org.clulab"                % "geonames"         % "1.0.0+20200518T005330Z.gadmworedas",
    "org.clulab"                % "glove-840b-300d"  % "0.1.0"
  )
}

lazy val core = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(eidoscommon % "compile->compile;test->test", ontologies)
  .aggregate(eidoscommon, ontologies, webapp)
  .settings(
    aggregate := false,
    compile / aggregate := true,
    test / aggregate := true
  )

// This prevents "error: recursive lazy value core needs type".
lazy val coreRef = LocalProject("core")

lazy val eidoscommon = project
  .settings (
    // This may not be used anymore. TODO
//    update / evictionWarningOptions := EvictionWarningOptions.default
//        .withWarnTransitiveEvictions(false)
//        .withWarnDirectEvictions(false)
  )

lazy val elasticsearch = project
  .dependsOn(eidoscommon)

lazy val ontologies = project
  .dependsOn(eidoscommon)

lazy val webapp = project
  .enablePlugins(PlayScala)
  .dependsOn(coreRef)

lazy val wmexchanger = project
  .dependsOn(eidoscommon)
