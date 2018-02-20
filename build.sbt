//import ReleaseTransformations._

name := "eidos"

scalaVersion := "2.12.4"

//EclipseKeys.withSource := true

libraryDependencies ++= {
  val procVer = "7.1.0"

  Seq(
    "org.clulab" %% "processors-main" % procVer,
    "org.clulab" %% "processors-corenlp" % procVer,
    "org.clulab" %% "processors-odin" % procVer,
    "org.clulab" %% "processors-modelsmain" % procVer,
    "org.clulab" %% "processors-modelscorenlp" % procVer,
    "org.scalatest" %% "scalatest" % "3.0.4" % "test",
    "ai.lum" %% "common" % "0.0.8",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"
  )
}

lazy val core = project in file(".")

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
/*
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  // releaseStepCommandAndRemaining("+test"),
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  ReleaseStep(action = Command.process("publishSigned", _)),
  // releaseStepCommandAndRemaining("+publishSigned"),
  setNextVersion,
  commitNextVersion,
  ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
  // releaseStepCommandAndRemaining("sonatypeReleaseAll"),
  pushChanges
)
*/

