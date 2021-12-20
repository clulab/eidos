import ReleaseTransformations._
import org.clulab.sbt.BuildUtils

ThisBuild / credentials ++= {
  val file = Path.userHome / ".sbt" / ".clulab-credentials"

  if (file.exists) Seq(Credentials(file))
  else Seq.empty
}

releaseProcess :=
    Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease
    ) ++
    Seq[ReleaseStep](releaseStepCommandAndRemaining(
      if (BuildUtils.useArtifactory) "+publish"
      else "+publishSigned"
    )) ++
    Seq[ReleaseStep](
      setNextVersion,
      commitNextVersion
    ) ++
    (
      if (BuildUtils.useArtifactory) Seq.empty[ReleaseStep]
      else Seq[ReleaseStep](releaseStepCommandAndRemaining("sonatypeReleaseAll"))
    ) ++
    Seq[ReleaseStep](
      pushChanges
    )

