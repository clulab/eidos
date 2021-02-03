
name := "eidos-eidoscommon"

resolvers ++= Seq(
  "Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release" // needed by processors-main
)

libraryDependencies ++= {
  val procVer = "8.2.3"

  Seq(
    "org.clulab"                 %% "processors-main"          % procVer,
    "org.clulab"                 %% "processors-corenlp"       % procVer,
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
