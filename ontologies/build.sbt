
name := "eidos-ontologies"

resolvers ++= Seq(
  "Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release" // org.clulab/glove-840b-300d needed by processors-main
)

libraryDependencies ++= {
  val  procVer = "8.2.3"

  Seq(
    "org.clulab"                 %% "processors-main"  % procVer,
    // local logging
    "ch.qos.logback"              % "logback-classic"  % "1.0.10",
    // dependency logging
    "org.apache.logging.log4j"    % "log4j-core"       % "2.12.0",
    // config
    "com.typesafe"                % "config"           % "1.4.0",
  )
}
