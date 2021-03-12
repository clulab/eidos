name := "eidos-eidoscommon"
description := "Code to be shared by other Eidos subprojects and clients"

resolvers ++= Seq(
  // This is needed by processors-main.
  ("Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release")
      // .withAllowInsecureProtocol(true)
)

libraryDependencies ++= {
  val procVer = "8.2.6"

  Seq(
    "org.clulab"                 %% "processors-corenlp"       % procVer,
    "org.clulab"                 %% "processors-main"          % procVer,
    "org.clulab"                 %% "processors-odin"          % procVer,
    // local logging
    "ch.qos.logback"              % "logback-classic"          % "1.0.10",
    "com.typesafe.scala-logging" %% "scala-logging"            % "3.7.2",
    // dependency logging
    "org.apache.logging.log4j"    % "log4j-core"               % "2.12.0",
    // config
    "com.typesafe"                % "config"                   % "1.4.0",
    "org.scalatest"              %% "scalatest"                % "3.0.4" % Test
  )
}
