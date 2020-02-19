
name := "kafka"
organization := "al.lum"

scalaVersion := "2.12.4"
crossScalaVersions := Seq("2.11.11", "2.12.4")

resolvers += "kafka" at "https://mvnrepository.com"

libraryDependencies ++= {
  val        json4sVersion = "3.5.2"
  val         kafkaVersion = "2.2.1"
  val     scalaTestVersion = "3.0.5"
  val embeddedKafkaVersion = "2.2.0"

  Seq(
    // local logging
    "ch.qos.logback"              % "logback-classic"   % "1.0.10",
    // dependency logging
    "org.apache.logging.log4j"    % "log4j-core"        % "2.12.0",
    // testing
    "org.scalatest"              %% "scalatest"         % scalaTestVersion % Test,
    // json
    "org.json4s"                 %% "json4s-core"       % json4sVersion,
    "org.json4s"                 %% "json4s-jackson"    % json4sVersion,
    // jena
    "org.apache.kafka"           %% "kafka"             % kafkaVersion,
    "org.apache.kafka"            % "kafka-clients"     % kafkaVersion,
    "org.apache.kafka"            % "kafka-streams"     % kafkaVersion,

    "io.github.embeddedkafka"    %% "embedded-kafka"    % embeddedKafkaVersion % Test,
    "jakarta.ws.rs"               % "jakarta.ws.rs-api" % "2.1.2" % Test
  )
}
