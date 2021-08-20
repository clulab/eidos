//enablePlugins(SiteScaladocPlugin)
enablePlugins(ScalaUnidocPlugin)
enablePlugins(GhpagesPlugin)

git.remoteRepo := "git@github.com:clulab/eidos.git"
ScalaUnidoc / siteSubdirName := "latest/api"
Compile / doc / scalacOptions ++= {
  // val currentBranch = git.gitCurrentBranch.value
  // val commitDate = git.gitHeadCommitDate.value.getOrElse("")
  // val docVersion = currentBranch + commitDate
  val docVersion = version.value

  Seq("-doc-title", "Eidos", "-doc-version", docVersion)
}

// How utest gets tangled up in this is a mystery, but this gets rid of it.
ScalaUnidoc / unidoc / scalacOptions ++= Seq("-skip-packages", "utest")
