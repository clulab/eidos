package org.clulab.sbt

import sbt._ // at

object Resolvers {
  val clulabResolver = "Artifactory" at "https://artifactory.cs.arizona.edu/artifactory/sbt-release"
  val jitpackResolver = "jitpack" at "https://jitpack.io"
  val localResolver = "Local Ivy Repository" at s"file://${System.getProperty( "user.home" )}/.ivy2/local"
}
