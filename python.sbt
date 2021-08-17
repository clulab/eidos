// See https://stackoverflow.com/questions/30205003/using-sbt-to-manage-projects-that-contain-both-scala-and-python

import scala.sys.process.Process

val testPythonTask = TaskKey[Unit]("testPython", "Run python tests")

testPythonTask := {
  // It should all be copied together into target/python, then they will have access
  val command = "pytest"
  val mainDirectory = new File("./src/main/python")
  val testDirectory = new File("./src/test/python")
  val workDirectory = new File("./target/python")
  val taskStreams: TaskStreams = streams.value
  taskStreams.log.info("Executing task testPython")
  println("Keith was here")
  Process(
    command,
    workDirectory // optional
//     optional system variables
//    "CLASSPATH" -> "path/to.jar",
//    "OTHER_SYS_VAR" -> "other_value"
  ) ! taskStreams.log
}
