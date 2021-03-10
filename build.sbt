// See also the other files in the project directory with sbt extensions.
// These are special additions for eidos that are named according to the
// task they are associated with: compile, release, test, etc.

name := "eidos"

// This is useful because timenorm loads a dll and only one dll is allowed per (Java) process.
// If it isn't here, sbt test (and run) can seemingly only be performed once before it will fail with
// java.lang.UnsatisfiedLinkError: no jnihdf5 in java.library.path
// The reasoning above needs to be reconsidered, because forking results in loss of menu
// functionality in things like EidosShell.  Output gets logged and, more importantly,
// input is blocked with null always being returned.
//fork := true

// Ensure that all the subprojects have the same settings for things like this.
lazy val commonSettings = Seq(
  organization := "org.clulab",
  scalaVersion := "2.12.4",
  crossScalaVersions := Seq("2.11.11", "2.12.4"),
  scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation"),
  compile / logLevel := Level.Warn,
  run / logLevel := Level.Warn
)

resolvers ++= Seq(
  "jitpack" at "https://jitpack.io", // needed by Versioner
  "Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release" // needed by processors-main, geonames, glove-840b-300d
)

libraryDependencies ++= {
  Seq(
    "org.clulab"               %% "timenorm"         % "1.0.5",
    "org.clulab"               %% "geonorm"          % "1.0.0",
    // This is used for config utilities in particular.
    "ai.lum"                   %% "common"           % "0.0.8",
    "com.github.WorldModelers"  % "Ontologies"       % "master-SNAPSHOT",
    // Web serialization needs this.  Match the plug-in version.
    "com.typesafe.play"        %% "play-json"        % "2.6.7",
    // This next one is used in MaaSUtils.
    "com.lihaoyi"              %% "upickle"          % "0.7.1",
    "org.scalatest"            %% "scalatest"        % "3.0.4"  % Test,
    "com.github.jsonld-java"    % "jsonld-java"      % "0.12.0" % Test,
    // These are not needed for the docker file if the cache is used.
    "org.clulab"                % "geonames"         % "1.0.0+20200518T005330Z.gadmworedas",
    "org.clulab"                % "glove-840b-300d"  % "0.1.0",
  )
}

lazy val core = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .aggregate(eidoscommon, ontologies, elasticsearch, wmexchanger, webapp)
  .dependsOn(eidoscommon % "compile->compile;test->test", ontologies)
  .settings(commonSettings)
  .settings(
    // The goal is to include compile and test only.
    assembly / aggregate := false,
    publish / aggregate := false,
    releaseProcess / aggregate := false
  )

// This prevents "error: recursive lazy value core needs type".
lazy val coreRef = LocalProject("core")

lazy val eidoscommon = project
  .settings(commonSettings)

lazy val elasticsearch = project
  .settings(commonSettings)
  .dependsOn(eidoscommon)

lazy val ontologies = project
  .settings(commonSettings)
  .dependsOn(eidoscommon)

lazy val webapp = project
  .enablePlugins(PlayScala)
  .settings(commonSettings)
  .dependsOn(coreRef)

lazy val wmexchanger = project
  .settings(commonSettings)
  .dependsOn(eidoscommon)
