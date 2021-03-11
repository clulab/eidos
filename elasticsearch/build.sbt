name := "eidos-elasticsearch"

libraryDependencies ++= {
  val json4sVersion = "3.5.2"

  Seq(
    // json
    "org.json4s"                 %% "json4s-core"      % json4sVersion,
    "org.json4s"                 %% "json4s-jackson"   % json4sVersion,
    // elasticsearch
    "com.amazonaws"               % "aws-java-sdk"                         % "1.11.592",
    "org.elasticsearch.client"    % "elasticsearch-rest-high-level-client" % "6.7.1",
    "org.apache.httpcomponents"   % "httpcore"                             % "4.4.9"
  )
}
