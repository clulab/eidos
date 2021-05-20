name := "wmexchanger"

// This is actually for the native packager.
maintainer := "msurdeanu@email.arizona.edu"

resolvers ++= Seq(
  ("Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release").withAllowInsecureProtocol(true), // processors-models
  "Local Ivy Repository" at s"file://${System.getProperty("user.home")}/.ivy2/local"
)

libraryDependencies ++= {
  // Versions were last checked 2021 Mar 12.
  val json4sVersion = "3.5.2" // Try to match processors.
  val  kafkaVersion = "2.4.0"

  Seq(
    // json
    "org.json4s"                 %% "json4s-core"      % json4sVersion, // up to 3.6.11
    "org.json4s"                 %% "json4s-jackson"   % json4sVersion, // up to 3.6.11
    // kafka
    "org.apache.kafka"           %% "kafka"            % kafkaVersion,  // up to 2.7.0
    "org.apache.kafka"            % "kafka-clients"    % kafkaVersion,  // up to 2.7.0
    // rest
    "org.apache.httpcomponents"   % "httpclient"       % "4.5.12",      // up to 4.5.13
    "org.apache.httpcomponents"   % "httpcore"         % "4.4.13",      // up to 4.4.14
    "org.apache.httpcomponents"   % "httpmime"         % "4.5.12"       // up to 4.5.13
  )
}
