
name := "eidos-elasticsearch"

libraryDependencies ++= {
  val json4sVersion = "3.5.2"

  Seq(
    // local logging
    "ch.qos.logback"              % "logback-classic"  % "1.0.10",
    // dependency logging
    "org.apache.logging.log4j"    % "log4j-core"       % "2.12.0",
    // json
    "org.json4s"                 %% "json4s-core"      % json4sVersion,
    "org.json4s"                 %% "json4s-jackson"   % json4sVersion,
    // elasticsearch
    "org.apache.httpcomponents"   % "httpcore"                             % "4.4.9",
    "com.amazonaws"               % "aws-java-sdk"                         % "1.11.592",
    "org.elasticsearch.client"    % "elasticsearch-rest-high-level-client" % "6.7.1"
  )
}

// This list should be updated any time one of the library dependencies above changes.
// If anything shows up as evicted, it is something new and should be investigated.
// One should also check periodically that nothing is included here unnecessarily.
dependencyOverrides ++= Seq(
  // These are suspected of being incompatible
  "org.apache.lucene"                % "lucene-queryparser"          % "7.7.0",
  "org.apache.lucene"                % "lucene-sandbox"              % "7.7.0",
  "org.apache.lucene"                % "lucene-queries"              % "7.7.0",
  "org.apache.lucene"                % "lucene-analyzers-common"     % "7.7.0",
  "org.apache.lucene"                % "lucene-core"                 % "7.7.0",
  "javax.xml.bind"                   % "jaxb-api"                    % "2.4.0-b180830.0359",
  "xml-apis"                         % "xml-apis"                    % "1.3.03",
  // These don't provoke incompatibility warnings.
  "joda-time"                        % "joda-time"                   %"2.10.1",
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-cbor"     % "2.8.11",
  "com.fasterxml.jackson.core"       % "jackson-core"                % "2.8.11",
  "org.yaml"                         % "snakeyaml"                   % "1.17",
  "com.amazonaws"                    % "aws-java-sdk-simpleworkflow" % "1.11.592",
  "org.apache.httpcomponents"        % "httpclient"                  % "4.5.5",
  "commons-logging"                  % "commons-logging"             % "1.1.3",
  "org.apache.httpcomponents"        % "httpcore"                    % "4.4.9",
  "com.typesafe"                     % "config"                      % "1.4.0",
  "org.apache.commons"               % "commons-lang3"               % "3.5",
  "org.slf4j"                        % "slf4j-api"                   % "1.7.12",
  "com.fasterxml.jackson.core"       % "jackson-databind"            % "2.8.4",
  "org.scala-lang.modules"          %% "scala-xml"                   % "1.0.6"
)
