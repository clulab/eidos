assembly / aggregate := false
assembly / assemblyMergeStrategy := {
  // See https://github.com/sbt/sbt-assembly.
  // This is nearly the same as case _ => MergeStrategy.defaultMergeStrategy with the most important difference
  // being that any problem noticed by deduplicate will halt the process.  The is presently/temporarily
  // preferred over a version that will silently handle new conflicts without alerting us to the potential problem.
  case PathList("META-INF", "DEPENDENCIES") => MergeStrategy.discard // All dependencies will be included in the assembly already.
  case PathList("META-INF", "MANIFEST.MF")  => MergeStrategy.discard // We'll make a new manifest for Eidos.
  case PathList("META-INF", "INDEX.LIST")   => MergeStrategy.discard // These can't be automatically combined, so skip the optimization.
  case PathList("module-info.class")        => MergeStrategy.discard // This might not be right, but it stops the complaints.

  // Rename everyone's licenses and notices.
  case PathList("LICENSE")                  => MergeStrategy.rename
  case PathList("META-INF", "LICENSE")      => MergeStrategy.rename
  case PathList("META-INF", "LICENSE.txt")  => MergeStrategy.rename
  case PathList("META-INF", "NOTICE")       => MergeStrategy.rename
  case PathList("META-INF", "NOTICE.txt")   => MergeStrategy.rename

  // If two copies of glove are included, this readme file will be duplicated.
  // case PathList("org", "clulab", "glove", "README.md") => MergeStrategy.concat
  // These all have different contents and cannot be automatically deduplicated.
  case PathList("reference.conf") => MergeStrategy.concat // Scala configuration files--important!
  case PathList("META-INF", "services", "org.apache.lucene.codecs.PostingsFormat")    => MergeStrategy.filterDistinctLines
  case PathList("META-INF", "services", "com.fasterxml.jackson.databind.Module")      => MergeStrategy.filterDistinctLines
  case PathList("META-INF", "services", "javax.xml.transform.TransformerFactory")     => MergeStrategy.first // or last or both?
  // Otherwise just keep one copy if the contents are the same and complain if not.
  case other => MergeStrategy.deduplicate
    // val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
    // oldStrategy(other)
}
// This prevents testing in core, then non-aggregation prevents it in other subprojects.
assembly / test := {}
