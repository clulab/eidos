name := "eidos-elasticsearch"

libraryDependencies ++= {
  // Versions were last checked 2021 Mar 12.
  val json4sVersion = "3.5.2"

  Seq(
    // json
    "org.json4s"                 %% "json4s-core"                          % json4sVersion, // Up to 3.6.11
    "org.json4s"                 %% "json4s-jackson"                       % json4sVersion, // Up to 3.6.11
    // elasticsearch
    "com.amazonaws"               % "aws-java-sdk"                         % "1.11.592",    // Up to 2.16.17
    "org.elasticsearch.client"    % "elasticsearch-rest-high-level-client" % "6.7.1",       // Up to 7.11.2
    "org.apache.httpcomponents"   % "httpcore"                             % "4.4.9"        // Up to 4.4.14
  )
}
