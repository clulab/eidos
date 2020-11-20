
name := "eidoscommon"
organization := "org.clulab"

scalaVersion := "2.12.4"
crossScalaVersions := Seq("2.11.11", "2.12.4")

resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies ++= {

  val procVer = "8.2.3"

  Seq(
    "org.clulab"                 %% "processors-main"          % procVer,
    "org.clulab"                 %% "processors-corenlp"       % procVer,
    "org.clulab"                 %% "processors-odin"          % procVer,
    // local logging
    "ch.qos.logback"              % "logback-classic"  % "1.0.10",
    // dependency logging
    "org.apache.logging.log4j"    % "log4j-core"       % "2.12.0",
    // config
    "com.typesafe"                % "config"           % "1.4.0",

  )
}
