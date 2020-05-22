package org.clulab.wm.eidos.serialization.web

import org.clulab.wm.eidos.BuildInfo
import play.api.libs.json.JsValue
import play.api.libs.json.Json

object BuildInfoObj {

  val mkJson: JsValue = Json.obj(
    "name" -> BuildInfo.name,
    "version" -> BuildInfo.version,
    "scalaVersion" -> BuildInfo.scalaVersion,
    "sbtVersion" -> BuildInfo.sbtVersion,
    "libraryDependencies" -> BuildInfo.libraryDependencies,
    "scalacOptions" -> BuildInfo.scalacOptions,
    "gitCurrentBranch" -> BuildInfo.gitCurrentBranch,
    "gitHeadCommit" -> BuildInfo.gitHeadCommit,
    "gitHeadCommitDate" -> BuildInfo.gitHeadCommitDate,
    "gitUncommittedChanges" -> BuildInfo.gitUncommittedChanges /* ,
    // These values change with each compilation and force repackaging.
    // Since they are not being used at all anyway, they are no longer included.
    // See build.sbt where a related line is commented out.
    "builtAtString" -> BuildInfo.builtAtString,
    "builtAtMillis" -> BuildInfo.builtAtMillis */
  )
}
