name := "akkaapp"

scalaVersion := "2.12.4" // This matches eidos

lazy val akkaVersion = "2.6.3"
lazy val akkaHttpVersion = "10.1.11"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
  "org.scalatest" %% "scalatest" % "3.1.0" % Test,
  
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion, // for swagger, 10.1.11 is latest
  "com.typesafe.akka" %% "akka-stream" % "2.6.3" // for swagger, which wanted 2.5.16
)
