import org.clulab.sbt.BuildUtils

val playVersion = BuildUtils.getProperty("./project/build.properties", "sbt-plugin.version")

// Given groupID % artifactID % revision % configuration, alphabetize by artifactID, groupID please.
// Latest version numbers were updated on 2021 Mar 11.
addSbtPlugin("com.eed3si9n"             % "sbt-assembly"         % "0.14.9")    // up to 0.15.0
addSbtPlugin("com.eed3si9n"             % "sbt-buildinfo"        % "0.7.0")     // up to 0.10.0
addSbtPlugin("net.virtual-void"         % "sbt-dependency-graph" % "0.9.2")     // up to 0.9.2
addSbtPlugin("com.typesafe.sbt"         % "sbt-ghpages"          % "0.6.3")     // up to 0.6.3
addSbtPlugin("com.typesafe.sbt"         % "sbt-git"              % "1.0.0")     // up to 1.0.0
addSbtPlugin("com.typesafe.play"        % "sbt-plugin"           % playVersion) // See build.properties.
// See https://index.scala-lang.org/sbt/sbt-pgp/sbt-pgp for details, especially version 2.0+.
addSbtPlugin("com.jsuereth"             % "sbt-pgp"              % "1.1.0")     // up to 1.1.2-1 *
addSbtPlugin("com.github.gseitz"        % "sbt-release"          % "1.0.13")    // up to 1.0.13
addSbtPlugin("com.typesafe.sbt"         % "sbt-site"             % "1.3.2")     // up to 1.4.1
addSbtPlugin("org.xerial.sbt"           % "sbt-sonatype"         % "2.3")       // up to 3.9.6 *
addSbtPlugin("com.typesafe.sbteclipse"  % "sbteclipse-plugin"    % "5.2.4")     // up to 5.2.4
// * Held back out of an abundance of caution.
