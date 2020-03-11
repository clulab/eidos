name := "restapp"

scalaVersion := "2.12.4"

// Must be here despite it being in core.
resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies ++= Seq(
  guice,
  // Why should this be required?
  "com.github.WorldModelers"   % "Ontologies"     % "master-SNAPSHOT",
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
)
