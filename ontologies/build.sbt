
name := "eidos-ontologies"

resolvers ++= Seq(
  "Artifactory" at "http://artifactory.cs.arizona.edu:8081/artifactory/sbt-release" // needed by processors-main
)

libraryDependencies ++= {
  val  procVer = "8.2.3"

  Seq(
    "org.clulab"                 %% "processors-main"  % procVer,
    // local logging
    "ch.qos.logback"              % "logback-classic"  % "1.0.10",
    // dependency logging
    "org.apache.logging.log4j"    % "log4j-core"       % "2.12.0",
    // config
    "com.typesafe"                % "config"           % "1.4.0"
  )
}

// This list should be updated any time one of the library dependencies above changes.
// If anything shows up as evicted, it is something new and should be investigated.
// One should also check periodically that nothing is included here unnecessarily.
dependencyOverrides ++= Seq(
  // These are suspected of being incompatible
  "javax.xml.bind"          % "jaxb-api"      % "2.4.0-b180830.0359",
  "xml-apis"                % "xml-apis"      % "1.3.03",
  // These don't provoke incompatibility warnings.
  "com.typesafe"            % "config"        % "1.4.0",
  "org.apache.commons"      % "commons-lang3" % "3.5",
  "org.slf4j"               % "slf4j-api"     % "1.7.12",
  "joda-time"               % "joda-time"     % "2.9.4",
  "org.scala-lang.modules" %% "scala-xml"     % "1.0.6"
)
