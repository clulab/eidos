name := "eidos"

version := "1.0"

scalaVersion := "2.11.8"

//EclipseKeys.withSource := true

libraryDependencies ++= {
  val procVer = "6.2.1-SNAPSHOT"

  Seq(
    "org.clulab" %% "processors-main" % procVer,
    "org.clulab" %% "processors-corenlp" % procVer,
    "org.clulab" %% "processors-odin" % procVer,
    "org.clulab" %% "processors-modelsmain" % procVer,
    "org.clulab" %% "processors-modelscorenlp" % procVer,
    "org.clulab" %% "processors-openie" % procVer,
    //"org.clulab" %% "influencer-core" % "0.1-SNAPSHOT",
    "org.scalatest" %% "scalatest" % "2.2.4" % "test",
    "ai.lum" %% "common" % "0.0.7",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
  )
}


lazy val core = project in file(".")

lazy val webapp = project
  .enablePlugins(PlayScala)
  .aggregate(core)
  .dependsOn(core)
