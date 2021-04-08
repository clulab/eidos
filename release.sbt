import ReleaseTransformations._
import org.clulab.sbt.BuildUtils

releaseProcess :=
    Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease
    ) +
    {
      if (BuildUtils.useArtifactory)
        releaseStepCommandAndRemaining("+publish")
      else
        releaseStepCommandAndRemaining("+publishSigned")
    } ++
    Seq(
      setNextVersion,
      commitNextVersion
    ) ++
    {
      if (BuildUtils.useArtifactory) Seq.empty
      else Seq(releaseStepCommandAndRemaining("sonatypeReleaseAll"))
    } +
    pushChanges

