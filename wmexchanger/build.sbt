name := "eidos-wmexchanger"

libraryDependencies ++= {
  // Versions were last checked 2021 Mar 12.
  val json4sVersion = "3.5.2"
  val  kafkaVersion = "2.4.0"

  Seq(
    // json
    "org.json4s"                 %% "json4s-core"      % json4sVersion, // Up to 3.6.11
    "org.json4s"                 %% "json4s-jackson"   % json4sVersion, // Up to 3.6.11
    // kafka
    "org.apache.kafka"           %% "kafka"            % kafkaVersion,  // Up to 2.7.0
    "org.apache.kafka"            % "kafka-clients"    % kafkaVersion,  // Up to 2.7.0
    // rest
    "org.apache.httpcomponents"   % "httpclient"       % "4.5.12",      // Up to 4.5.13
    "org.apache.httpcomponents"   % "httpcore"         % "4.4.13",      // Up to 4.4.14
    "org.apache.httpcomponents"   % "httpmime"         % "4.5.12"       // Up to 4.5.13
  )
}
