
name := "eidos-eidoscommon"

resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies ++= {
  val procVer = "8.2.3"

  Seq(
    "org.clulab"                 %% "processors-main"          % procVer,
    "org.clulab"                 %% "processors-corenlp"       % procVer,
    "org.clulab"                 %% "processors-odin"          % procVer,
    // local logging
    "ch.qos.logback"              % "logback-classic"          % "1.0.10",
    // dependency logging
    "org.apache.logging.log4j"    % "log4j-core"               % "2.12.0",
    // config
    "com.typesafe"                % "config"                   % "1.4.0",
    "org.scalatest"              %% "scalatest"                % "3.0.4"  // % "test"
  )
}
