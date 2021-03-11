name := "eidos-webapp"
description := "A web application providing a user interface to Eidos"

// This shouldn't be necessary.
resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies ++= Seq(
  guice,
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
)
