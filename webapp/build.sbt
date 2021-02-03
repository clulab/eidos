name := """eidos-webapp"""

resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies ++= Seq(
  guice,
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test,
  "com.github.clulab"       % "timenorm"           % "timenorm-0.9.6.14" exclude("org.slf4j", "slf4j-log4j12")
)

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "org.clulab.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "org.clulab.binders._"

// This list should be updated any time one of the library dependencies above changes.
// If anything shows up as evicted, it is something new and should be investigated.
// One should also check periodically that nothing is included here unnecessarily.
dependencyOverrides ++= Seq(
  // These are suspected of being incompatible
  "com.google.guava"               % "guava"                    % "22.0",
  "com.typesafe.akka"             %% "akka-stream"              % "2.5.6",
  "com.typesafe.akka"             %% "akka-actor"               % "2.5.6",
  "com.typesafe.play"             %% "play-json"                % "2.7.4",
  "org.apache.lucene"              % "lucene-queryparser"       % "6.6.6",
  "org.apache.lucene"              % "lucene-queries"           % "6.6.6",
  "org.apache.lucene"              % "lucene-analyzers-common"  % "6.6.6",
  "org.apache.lucene"              % "lucene-core"              % "6.6.6",
  "org.scala-lang.modules"        %% "scala-xml"                % "1.0.6",
  "commons-io"                     % "commons-io"               % "2.6",
  "javax.xml.bind"                 % "jaxb-api"                 % "2.4.0-b180830.0359",
  "xml-apis"                       % "xml-apis"                 % "1.3.03",
  // These don't provoke incompatibility warnings.
  "org.apache.commons"             % "commons-compress"         % "1.16.1",
  "org.objenesis"                  % "objenesis"                % "2.6",
  "org.apache.commons"             % "commons-math3"            % "3.4.1",
  "org.scalaz"                    %% "scalaz-core"              % "7.2.8",
  "org.scala-lang.modules"        %% "scala-parser-combinators" % "1.0.6",
  "org.apache.commons"             % "commons-lang3"            % "3.6",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jsr310"  % "2.9.8",
  "com.fasterxml.jackson.datatype" % "jackson-datatype-jdk8"    % "2.9.8",
  "com.fasterxml.jackson.core"     % "jackson-databind"         % "2.9.8",
  "com.fasterxml.jackson.core"     % "jackson-annotations"      % "2.9.8",
  "com.fasterxml.jackson.core"     % "jackson-core"             % "2.9.8",
  "joda-time"                      % "joda-time"                % "2.10.1",
  "com.typesafe"                   % "config"                   % "1.4.0",
  "org.yaml"                       % "snakeyaml"                % "1.14",
  "org.slf4j"                      % "slf4j-api"                % "1.7.12",
  "ch.qos.logback"                 % "logback-classic"          % "1.2.3",
  "ch.qos.logback"                 % "logback-core"             % "1.2.3"
)
