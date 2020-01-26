name := """restapp"""
scalaVersion := "2.12.4"

// Must be here despite it being in core.
resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies ++= Seq(
  guice,
  "com.github.WorldModelers"   % "Ontologies"     % "master-SNAPSHOT",
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test,
)

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "org.clulab.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "org.clulab.binders._"
