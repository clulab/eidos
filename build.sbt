name := "eidos"

version := "1.0"

scalaVersion := "2.12.4"

//EclipseKeys.withSource := true

libraryDependencies ++= {
  val procVer = "7.0.0"

  Seq(
    "org.clulab" %% "processors-main" % procVer,
    "org.clulab" %% "processors-corenlp" % procVer,
    "org.clulab" %% "processors-odin" % procVer,
    "org.clulab" %% "processors-modelsmain" % procVer,
    "org.clulab" %% "processors-modelscorenlp" % procVer,
    "org.clulab" %% "processors-openie" % procVer,
    //"org.clulab" %% "influencer-core" % "0.1-SNAPSHOT",
    //"edu.stanford.nlp"    %  "stanford-corenlp"  % "3.8.0" classifier "models"
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
