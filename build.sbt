import ReleaseTransformations._
import Tests._

name := "eidos"
organization := "org.clulab"

fork := true

scalaVersion := "2.12.4"
crossScalaVersions := Seq("2.11.11", "2.12.4")

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

resolvers ++= Seq(
  "jitpack" at "https://jitpack.io", // com.github.WorldModelers/Ontologies
  "Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release" // org.clulab/glove-840b-300d
)

libraryDependencies ++= {
  val      procVer = "8.2.3"
  val procModelVer = "7.5.4"
  val    luceneVer = "6.6.6"
  val   lihaoyiVer = "0.7.1"

  Seq(
    "org.clulab"                 %% "processors-main"          % procVer,
    "org.clulab"                 %% "processors-corenlp"       % procVer,
    "org.clulab"                 %% "processors-odin"          % procVer,
    "org.clulab"                 %% "processors-modelsmain"    % procModelVer,
    "org.clulab"                 %% "processors-modelscorenlp" % procModelVer,
    "org.clulab"                 %% "geonorm"                  % "1.0.0",
    "org.clulab"                  % "geonames"                 % "1.0.0+20200518T005330Z.gadmworedas",
    "org.clulab"                 %% "timenorm"                 % "1.0.5",
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

sourceGenerators in Compile += Def.task {
  import java.io.File
  import Versioner._
  // These values need to be collected in a task in order have them forwarded to Scala functions.
  val versioner = Versioner(git.runner.value, git.gitCurrentBranch.value, baseDirectory.value, (sourceManaged in Compile).value)

  // The user should set these values.
  val codeDir = "src/main/resources/"
  val ontologyDir = codeDir + "org/clulab/wm/eidos/english/ontologies/"
  val namespace = "com.github.clulab.eidos"

  val files = new File(ontologyDir)
      .listFiles
      .filter { file => file.isFile }
      .map { file => ontologyDir + file.name }

  versioner.version(namespace, files)
}.taskValue

Test / fork := true // Also forces sequential operation
Test / parallelExecution := false // Keeps groups in their order   false then true worked 4:14 and portuguese last
//Test / testForkedParallel := true // Allow parallel within group?

{
  def groupByLanguage(tests: Seq[TestDefinition]) = {
    //def newRunPolicy = SubProcess(ForkOptions())
    def newRunPolicy = InProcess

    val englishTests = tests.filter(_.name.contains(".text.english."))
    val portugueseTests = tests.filter(_.name.contains(".text.portuguese."))
    val languageNames = englishTests.map(_.name) ++ portugueseTests.map(_.name)
    val otherTests = tests.filter(test => !languageNames.contains(test.name))
    val allNames = otherTests.map(_.name) ++ languageNames
//    val otherAndEnglishGroup = new Group("otherAndEnglish", otherTests ++ englishTests, newWubProcess) 
    val englishGroup = new Group("english", englishTests, newRunPolicy)
    val portugueseGroup = new Group("portuguese", portugueseTests, newRunPolicy)
    val otherGroup = new Group("other", otherTests, newRunPolicy)

    Seq(otherGroup, englishGroup, portugueseGroup)
  }

  testGrouping in Test := groupByLanguage((definedTests in Test).value)
}


// This is useful because timenorm loads a dll and only one dll is allowed per (Java) process.
// If it isn't here, sbt test can seemingly only be run once before it will fail with
// java.lang.UnsatisfiedLinkError: no jnihdf5 in java.library.path
// Caused by: java.lang.UnsatisfiedLinkError: Native Library jnihdf5.dll already loaded in another classloader
// However, this also doubles the testing time, so it is disabled here.  Enable it if the exception appears.
// The value of fork is also set above to preserve order, so this remains only for documentation purposes.
// fork := true

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

// account for dependency on glove vector file
pomIncludeRepository := { (repo: MavenRepository) =>
  repo.root.startsWith("http://artifactory.cs.arizona.edu")
}

scmInfo := Some(
  ScmInfo(
    url("https://github.com/clulab/eidos"),
    "scm:git:https://github.com/clulab/eidos.git"
  )
)

licenses := List("Apache License, Version 2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.html"))

homepage := Some(url("https://github.com/clulab/eidos"))

developers := List(
  Developer(
    id    = "mihai.surdeanu",
    name  = "Mihai Surdeanu",
    email = "mihai@surdeanu.info",
    url   = url("http://surdeanu.info/mihai/")
  )
)
//
// end publishing settings
//

lazy val core = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
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

lazy val webapp = project
  .enablePlugins(PlayScala)
  .aggregate(core)
  .dependsOn(core)

lazy val elasticsearch = project

lazy val wmexchanger = project

test in assembly := {}
assemblyMergeStrategy in assembly := {
  // See https://github.com/sbt/sbt-assembly.
  // This is nearly the same as case _ => MergeStrategy.defaultMergeStrategy with the most important difference
  // being that any problem noticed by deduplicate will halt the process.  The is presently/temporarily
  // preferred over a version that will silently handle new conflicts without alerting us to the potential problem.
  case PathList("META-INF", "MANIFEST.MF")  => MergeStrategy.discard // We'll make a new manifest for Eidos.
  case PathList("META-INF", "DEPENDENCIES") => MergeStrategy.discard // All dependencies will be included in the assembly already.
  case PathList("module-info.class")        => MergeStrategy.discard // This might not be right, but it stops the complaints.
  case PathList("META-INF", "LICENSE")      => MergeStrategy.concat  // Concatenate everyones licenses and notices.
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

