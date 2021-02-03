
name := "eidos-wmexchanger"

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

// This list should be updated any time one of the library dependencies above changes.
// If anything shows up as evicted, it is something new and should be investigated.
// One should also check periodically that nothing is included here unnecessarily.
dependencyOverrides ++= Seq(
   // These are suspected of being incompatible
  "com.typesafe.scala-logging" %% "scala-logging"       % "3.9.2",
  "javax.xml.bind"              % "jaxb-api"            % "2.4.0-b180830.0359",
  "xml-apis"                    % "xml-apis"            % "1.3.03",
   // These don't provoke incompatibility warnings.
  "com.fasterxml.jackson.core"  % "jackson-databind"    % "2.10.0",
  "com.fasterxml.jackson.core"  % "jackson-core"        % "2.10.0",
  "com.fasterxml.jackson.core"  % "jackson-annotations" % "2.10.0",
  "org.slf4j"                   % "slf4j-api"           % "1.7.28",
  "com.typesafe"                % "config"              % "1.4.0",
  "org.apache.commons"          % "commons-lang3"       % "3.5",
  "joda-time"                   % "joda-time"           % "2.9.4",
  "org.scala-lang.modules"     %% "scala-xml"           % "1.0.6"
)
