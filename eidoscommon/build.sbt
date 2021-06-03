import org.clulab.sbt.Resolvers

name := "eidos-eidoscommon"
description := "Code to be shared by other Eidos subprojects and clients"

resolvers ++= Seq(
  Resolvers.localResolver, // Reserve for Two Six.
  Resolvers.clulabResolver // processors-models, transitive dependency
)

libraryDependencies ++= {
  // Versions were last checked 2021 Mar 12.
  val procVer = "8.3.6"

  Seq(
    "org.clulab"                 %% "processors-corenlp"       % procVer,       // up to 8.3.6
    "org.clulab"                 %% "processors-main"          % procVer,       // up to 8.3.6
    // local logging
    "ch.qos.logback"              % "logback-classic"          % "1.0.10",      // up to 1.2.
    "com.typesafe.scala-logging" %% "scala-logging"            % "3.7.2",       // up to 3.9.2
    // dependency logging
    "org.apache.logging.log4j"    % "log4j-core"               % "2.12.0",      // up to 2.14.0
    // config
    "com.typesafe"                % "config"                   % "1.4.0",       // up to 1.4.1
    "org.scalatest"              %% "scalatest"                % "3.2.5" % Test // up to 3.2.5
  )
}
