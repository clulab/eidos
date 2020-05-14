
name := "wmexchanger"
organization := "org.clulab"

scalaVersion := "2.12.4"
crossScalaVersions := Seq("2.11.11", "2.12.4")

libraryDependencies ++= {
  val json4sVersion = "3.5.2"
  val  kafkaVersion = "2.4.0"

  Seq(
    // local logging
    "ch.qos.logback"              % "logback-classic"  % "1.0.10",
    // dependency logging
    "org.apache.logging.log4j"    % "log4j-core"       % "2.12.0",
    // json
    "org.json4s"                 %% "json4s-core"      % json4sVersion,
    "org.json4s"                 %% "json4s-jackson"   % json4sVersion,
    // kafka
    "org.apache.kafka"           %% "kafka"            % kafkaVersion,
    "org.apache.kafka"            % "kafka-clients"    % kafkaVersion,
    // config
    "com.typesafe"                % "config"           % "1.4.0",
    // rest
    "org.apache.httpcomponents"   % "httpcore"         % "4.4.13",
    "org.apache.httpcomponents"   % "httpclient"       % "4.5.12",
    "org.apache.httpcomponents"   % "httpmime"         % "4.5.12"
  )
}
