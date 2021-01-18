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

// See https://www.scala-sbt.org/1.x/docs/Multi-Project.html for the reason this can't be in
// a separate file: "The definitions in .sbt files are not visible in other .sbt files."
lazy val assemblySettings = Seq(
  test in assembly := {},

  assemblyMergeStrategy in assembly := {
    // See https://github.com/sbt/sbt-assembly.
    // This is nearly the same as case _ => MergeStrategy.defaultMergeStrategy with the most important difference
    // being that any problem noticed by deduplicate will halt the process.  The is presently/temporarily
    // preferred over a version that will silently handle new conflicts without alerting us to the potential problem.
    case PathList("META-INF", "MANIFEST.MF")  => MergeStrategy.discard // We'll make a new manifest for Eidos.
    case PathList("META-INF", "DEPENDENCIES") => MergeStrategy.discard // All dependencies will be included in the assembly already.
    case PathList("module-info.class")        => MergeStrategy.discard // This might not be right, but it stops the complaints.
    case PathList("META-INF", "LICENSE")      => MergeStrategy.concat  // Concatenate everyone's licenses and notices.
    case PathList("META-INF", "LICENSE.txt")  => MergeStrategy.concat
    case PathList("META-INF", "NOTICE")       => MergeStrategy.concat
    case PathList("META-INF", "NOTICE.txt")   => MergeStrategy.concat
    // These all have different contents and cannot be automatically deduplicated.
    case PathList("META-INF", "services", "org.apache.lucene.codecs.PostingsFormat") => MergeStrategy.filterDistinctLines
    case PathList("META-INF", "services", "com.fasterxml.jackson.databind.Module")   => MergeStrategy.filterDistinctLines
    case PathList("META-INF", "services", "javax.xml.transform.TransformerFactory")  => MergeStrategy.first // or last or both?
    case PathList("reference.conf") => MergeStrategy.concat // Scala configuration files--important!
    // Otherwise just keep one copy if the contents are the same and complain if not.
    case _ => MergeStrategy.deduplicate
  }
)

lazy val publishSettings = {
  import sbt.Keys.{developers, homepage, licenses, scmInfo}
  import sbt.url
  
  Seq(
    // publish to a maven repo
    publishMavenStyle := true,
    // the standard maven repository
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    // Let’s remove any repositories for optional dependencies in our artifact.
    pomIncludeRepository := { (repo: MavenRepository) =>
      repo.root.startsWith("http://artifactory.cs.arizona.edu")
    },
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/clulab/eidos"),
        "scm:git:https://github.com/clulab/eidos.git"
      )
    ),
    licenses := List("Apache License, Version 2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.html")),
    homepage := Some(url("https://github.com/clulab/eidos")),
    developers := List(
      Developer(
        id    = "mihai.surdeanu",
        name  = "Mihai Surdeanu",
        email = "mihai@surdeanu.info",
        url   = url("http://surdeanu.info/mihai/")
      )
    )
  )
}

// This is build.sbt after all.
lazy val buildSettings = Seq(
  buildInfoPackage := "org.clulab.wm.eidos",
  // This next line of code results in constantly changing source files which then require
  // constant repackaging.  Absent an active use case, BuildTime is therefore skipped.
  // buildInfoOptions += BuildInfoOption.BuildTime,
  buildInfoKeys := Seq[BuildInfoKey](
    name, version, scalaVersion, sbtVersion, libraryDependencies, scalacOptions,
    "gitCurrentBranch" -> { git.gitCurrentBranch.value },
    "gitHeadCommit" -> { git.gitHeadCommit.value.getOrElse("") },
    "gitHeadCommitDate" -> { git.gitHeadCommitDate.value.getOrElse("") },
    "gitUncommittedChanges" -> { git.gitUncommittedChanges.value }
  )
)

// Ensure that all the subprojects have the same settings for things like this.
lazy val commonSettings = Seq(
  organization := "org.clulab",
  scalaVersion := "2.12.4",
  crossScalaVersions := Seq("2.11.11", "2.12.4"),
  scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation"),
  logLevel in run := Level.Warn
) ++ buildSettings ++ assemblySettings ++ publishSettings

resolvers ++= Seq(
  "jitpack" at "https://jitpack.io", // needed by Versioner
  "Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release" // needed by processors-main, geonames, glove-840b-300d
)

libraryDependencies ++= {
  val      procVer = "8.2.5-SNAPSHOT"
  val procModelVer = "7.5.4"
  val    luceneVer = "6.6.6"
  val   lihaoyiVer = "0.7.1"

  Seq(
    "org.clulab"                 %% "processors-main"          % procVer,
    "org.clulab"                 %% "processors-corenlp"       % procVer,
    "org.clulab"                 %% "processors-odin"          % procVer,
    "org.clulab"                 %% "processors-modelsmain"    % procModelVer,
    "org.clulab"                 %% "processors-modelscorenlp" % procModelVer,
    "org.clulab"                 %% "timenorm"                 % "1.0.5",
    "org.clulab"                 %% "geonorm"                  % "1.0.0",	
    // This one below is not needed for the docker file if the cache is used.
    "org.clulab"                  % "geonames"                 % "1.0.0+20200518T005330Z.gadmworedas",
    // This one below is not needed for the docker file if the cache is used.
    "org.clulab"                  % "glove-840b-300d"          % "0.1.0",
    "ai.lum"                     %% "common"                   % "0.0.8",
    "org.scalatest"              %% "scalatest"                % "3.0.4" % "test",
    "commons-io"                  % "commons-io"               % "2.5",
    "com.typesafe"                % "config"                   % "1.3.1",
    "net.sf.saxon"                % "saxon-dom"                % "8.7",
    "org.slf4j"                   % "slf4j-api"                % "1.7.10",
    "com.github.jsonld-java"      % "jsonld-java"              % "0.12.0",
    "com.github.WorldModelers"    % "Ontologies"               % "master-SNAPSHOT",
    "com.typesafe.scala-logging" %% "scala-logging"            % "3.7.2",
    "com.typesafe.play"          %% "play-json"                % "2.6.7", // match the plug-in
    "org.apache.lucene"           % "lucene-core"              % luceneVer,
    "org.apache.lucene"           % "lucene-analyzers-common"  % luceneVer,
    "org.apache.lucene"           % "lucene-queryparser"       % luceneVer,
    "org.apache.lucene"           % "lucene-grouping"          % luceneVer,
    "com.lihaoyi"                %% "ujson"                    % lihaoyiVer,
    "com.lihaoyi"                %% "upickle"                  % lihaoyiVer
  )
}

lazy val core = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .aggregate(eidoscommon, ontologies, elasticsearch, wmexchanger, webapp)
  .dependsOn(eidoscommon, ontologies)
  .settings(commonSettings: _*)
  .settings(
    // The goal is to include compile and test only.
    assembly / aggregate := false,
    publish / aggregate := false,
    releaseProcess / aggregate := false
  )

// This prevents "error: recursive lazy value core needs type".
lazy val coreRef = LocalProject("core")

lazy val eidoscommon = project
  .settings(commonSettings: _*)

lazy val elasticsearch = project
  .settings(commonSettings: _*)
  .dependsOn(eidoscommon)

lazy val ontologies = project
  .settings(commonSettings: _*)
  .dependsOn(eidoscommon)

lazy val webapp = project
  .settings(commonSettings: _*)
  .enablePlugins(PlayScala)
  .dependsOn(coreRef)

lazy val wmexchanger = project
  .settings(commonSettings: _*)
  .dependsOn(eidoscommon)

git.remoteRepo := {
  // scaladoc hosting
  enablePlugins(SiteScaladocPlugin)
  enablePlugins(GhpagesPlugin)

  "git@github.com:clulab/eidos.git"
}
