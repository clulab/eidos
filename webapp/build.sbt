name := """eidos-webapp"""

//resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies ++= Seq(
  guice,
  "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test,
  "com.github.clulab"       % "timenorm"           % "timenorm-0.9.6.14" exclude("org.slf4j", "slf4j-log4j12")
)
