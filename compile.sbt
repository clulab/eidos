import org.clulab.sbt.Versioner

// This setting adversely affects logLevel during tests, so needs counterweight.
// ThisBuild / Compile / logLevel := Level.Error
// ThisBuild / Runtime / logLevel := Level.Info
ThisBuild / Compile / scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")
Compile / sourceGenerators += Def.task {
  import java.io.File
  import Versioner._
  // These values need to be collected in a task in order have them forwarded to Scala functions.
  val versioner = Versioner(git.runner.value, git.gitCurrentBranch.value, baseDirectory.value, (Compile / sourceManaged).value)

  // The user should set these values.
  val codeDir = "src/main/resources/"
  val ontologyDir = codeDir + "org/clulab/wm/eidos/english/ontologies/"
  val namespace = "com.github.clulab.eidos"

  val files = new File(ontologyDir)
      .listFiles
      .filter { file => file.isFile }
      .map { file => ontologyDir + file.name }

  versioner.version(namespace, files)
}.taskValue
