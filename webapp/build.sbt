name := """eidos-webapp"""

resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies ++= Seq(
  guice,
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test,
  "com.github.clulab" % "timenorm" % "timenorm-0.9.6.14" exclude("org.slf4j", "slf4j-log4j12")
)

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "org.clulab.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "org.clulab.binders._"
