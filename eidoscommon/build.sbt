import org.clulab.sbt.Resolvers

name := "eidos-eidoscommon"
description := "Code to be shared by other Eidos subprojects and clients"

resolvers ++= Seq(
  Resolvers.localResolver, // Reserve for Two Six.
  Resolvers.clulabResolver // processors-models, transitive dependency
)

libraryDependencies ++= {
  // Versions were last checked 2021 Mar 12.
  val procVer = "8.4.5"
  val luceneVer = "6.6.6"

  // Run dependencyTree, dependencyList, and dependencyStats on eidos and
  // see what kinds of duplicates with different version numbers show up.
  Seq(
    // Processors brings in an old version of lucene via stanford.nlp.
    // That version of lucene will be updated in the main eidos project to 6.6.6.
    // It is possible then that something using eidoscommons and something using
    // eidos would get different answers.
    "org.apache.lucene"           % "lucene-analyzers-common"  % luceneVer,
    "org.apache.lucene"           % "lucene-core"              % luceneVer,
    "org.apache.lucene"           % "lucene-queries"           % luceneVer,
    "org.apache.lucene"           % "lucene-queryparser"       % luceneVer,
    "org.apache.lucene"           % "lucene-sandbox"           % luceneVer,
    // The common-io 2.5 that processors uses is going to get evicted anyway,
    // so add version 2.6 here to prevent ending up with both in places.
    "commons-io"                  % "commons-io"               % "2.6",
    // Newer version of these are used in the play framework.
    // That means that changes to play need to be tracked here.
    "com.fasterxml.jackson.core"  % "jackson-annotations"      % "2.9.8",
    "com.fasterxml.jackson.core"  % "jackson-databind"         % "2.9.8",
    "joda-time"                   % "joda-time"                % "2.10.1",
    // json4s uses an older version of this.  Upgrade it.
    "org.scala-lang.modules"     %% "scala-xml"                % "1.2.0",

    // Use what is left of processors and transitive dependencies.
    "org.clulab"                 %% "processors-corenlp"       % procVer,       // up to 8.4.1
    "org.clulab"                 %% "processors-main"          % procVer,       // up to 8.4.1
    // local logging
    "ch.qos.logback"              % "logback-classic"          % "1.2.8",       // up to 1.2.8; less than 1.2 is vulnerable
    "com.typesafe.scala-logging" %% "scala-logging"            % "3.7.2",       // up to 3.9.2
    // config
    "com.typesafe"                % "config"                   % "1.4.0",       // up to 1.4.1
    "org.scalatest"              %% "scalatest"                % "3.2.5" % Test // up to 3.2.5
  )
}
