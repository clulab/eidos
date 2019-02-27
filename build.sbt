import ReleaseTransformations._
import Tests._

name := "eidos"
organization := "org.clulab"

scalaVersion := "2.12.4"
crossScalaVersions := Seq("2.11.11", "2.12.4")

resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies ++= {
  val procVer = "7.4.2"

  Seq(
    "org.clulab"    %% "processors-main"          % procVer,
    "org.clulab"    %% "processors-corenlp"       % procVer,
    "org.clulab"    %% "processors-odin"          % procVer,
    "org.clulab"    %% "processors-modelsmain"    % procVer,
    "org.clulab"    %% "processors-modelscorenlp" % procVer,
    "org.clulab"    % "geonorm-models"            % "0.9.0",
    "ai.lum"        %% "common"                   % "0.0.8",
    "org.scalatest" %% "scalatest"                % "3.0.4" % "test",
    "commons-io"    %  "commons-io"               % "2.5",
    "com.typesafe"  %  "config"                   % "1.3.1",
    "net.sf.saxon"  % "saxon-dom"                 % "8.7",
    "org.slf4j"     % "slf4j-api"                 % "1.7.10",
    "com.github.jsonld-java"     % "jsonld-java"    % "0.12.0",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",

    "org.deeplearning4j" % "deeplearning4j-modelimport" % "1.0.0-beta2",
    "org.deeplearning4j" % "deeplearning4j-nn" % "1.0.0-beta2",
    "org.nd4j" % "nd4j-native-platform" % "1.0.0-beta2",
    "com.typesafe.play" % "play-json_2.12" % "2.6.9",
    "com.al333z" % "anti-xml_2.12" % "0.7.11",
    "org.tensorflow" % "tensorflow" % "1.12.0"
  )
}

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


//libraryDependencies ++= {
//  val (major, minor) = CrossVersion.partialVersion(scalaVersion.value).get
//  val timenorm = "timenorm-0.9.6.15" + (if (minor == 11) "_2.11.11" else "")
//
//  Seq("com.github.clulab" % "timenorm" % timenorm exclude("org.slf4j", "slf4j-log4j12"))
//}

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
    case "META-INF/services/org.nd4j.linalg.factory.Nd4jBackend" => MergeStrategy.first
    case "META-INF/services/org.nd4j.linalg.compression.NDArrayCompressor" => MergeStrategy.first
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
