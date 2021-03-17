name := "eidos-eidoscommon"
description := "Code to be shared by other Eidos subprojects and clients"

resolvers ++= Seq(
  // This is needed by processors-main.
  ("Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release")
      // .withAllowInsecureProtocol(true)
)

libraryDependencies ++= {
  // Versions were last checked 2021 Mar 12.
  val procVer = "8.2.7-SNAPSHOT"

  Seq(
    "org.clulab"                 %% "processors-corenlp"       % procVer,       // Up to 8.2.6
    "org.clulab"                 %% "processors-main"          % procVer,       // Up to 8.2.6
    "org.clulab"                 %% "processors-odin"          % procVer,       // Up to 8.2.6
    // local logging
    "ch.qos.logback"              % "logback-classic"          % "1.0.10",      // Up to 1.2.
    "com.typesafe.scala-logging" %% "scala-logging"            % "3.7.2",       // Up to 3.9.2
    // dependency logging
    "org.apache.logging.log4j"    % "log4j-core"               % "2.12.0",      // Up to 2.14.0
    // config
    "com.typesafe"                % "config"                   % "1.4.0",       // Up to 1.4.1
    "org.scalatest"              %% "scalatest"                % "3.0.4" % Test // Up to 3.2.5
  )
}
