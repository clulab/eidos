
name := "eidos-wmexchanger"

libraryDependencies ++= {
  val json4sVersion = "3.5.2"
  val  kafkaVersion = "2.4.0"

  Seq(
    // json
    "org.json4s"                 %% "json4s-core"      % json4sVersion,
    "org.json4s"                 %% "json4s-jackson"   % json4sVersion,
    // kafka
    "org.apache.kafka"           %% "kafka"            % kafkaVersion,
    "org.apache.kafka"            % "kafka-clients"    % kafkaVersion,
    // rest
    "org.apache.httpcomponents"   % "httpcore"         % "4.4.13",
    "org.apache.httpcomponents"   % "httpclient"       % "4.5.12",
    "org.apache.httpcomponents"   % "httpmime"         % "4.5.12"
  )
}
