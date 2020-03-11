
name := "sparql"
organization := "al.lum"

scalaVersion := "2.12.4"
crossScalaVersions := Seq("2.11.11", "2.12.4")

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
    // jena
    "org.apache.jena"            % "apache-jena-libs"  % "3.14.0" pomOnly()
  )
}
