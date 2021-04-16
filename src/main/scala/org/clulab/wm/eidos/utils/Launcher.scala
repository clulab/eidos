package org.clulab.wm.eidos.utils

import org.clulab.wm.eidoscommon.utils.Counter
import org.clulab.wm.eidoscommon.utils.Holder
import org.clulab.wm.eidoscommon.utils.Logging

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuilder

class Launcher(val args: Array[String]) {
  protected val counter: Counter = Counter()
  protected val waitTime = 30000

  def getCount: Int = counter.get

  def newProcessBuilder(): ProcessBuilder = {
    val processBuilder = new ProcessBuilder(args.toList.asJava)

    processBuilder.redirectInput(ProcessBuilder.Redirect.INHERIT)
    processBuilder.redirectOutput(ProcessBuilder.Redirect.INHERIT)
    processBuilder.redirectError(ProcessBuilder.Redirect.INHERIT)
    processBuilder
  }

  protected def launchOnce(): Int = {
    val processBuilder = newProcessBuilder()
    val command = args.mkString(" ")
    Launcher.logger.info(s"""The laucher is running "$command".""")
    val process = processBuilder.start()
    process.waitFor()
  }

  // If retry is true, runs the program until it returns 0.
  // If it is false, runs it just until it returns.
  // Either way, it returns the result of the program.
  def launch(retry: Boolean = true): Int = {
    val resultHolder = new Holder[Int](0)

    while ({
      if (counter.get > 0)
        Thread.sleep(waitTime)
      counter.inc()
      resultHolder.set(launchOnce()) != 0 && retry
    }) { }
    resultHolder.get
  }
}

object Launcher extends Logging {

  def apply(args: Array[String]): Launcher = new Launcher(args)

  def main(args: Array[String]): Unit = {
    if (args.length > 0)
      Launcher(args).launch(true)
  }

  def isInt(text: String): Boolean = {
    try {
      text.toInt
      true
    }
    catch {
      case _: java.lang.NumberFormatException => false
    }
  }
}

class JavaLauncher(classname: String, programArgs: Array[String], javaArgs: Array[String]) extends Launcher(JavaLauncher.mkArgs(classname, programArgs, javaArgs))

object JavaLauncher {

  protected def mkArgs(classname: String, programArgs: Array[String], javaArgs: Array[String]): Array[String] = {
    // Also configure garbage collector for best type and any settings it needs.
    val arrayBuilder = ArrayBuilder.make[String]

    arrayBuilder += "java"
    arrayBuilder += "-Dfile.encoding=UTF-8"
    arrayBuilder += "-classpath"
    arrayBuilder += System.getProperty("java.class.path")
    arrayBuilder ++= javaArgs
    arrayBuilder += classname
    arrayBuilder ++= programArgs
    arrayBuilder.result
  }

  def apply(classname: String, programArgs: Array[String], javaArgs: Array[String]): JavaLauncher = new JavaLauncher(classname, programArgs, javaArgs)

  def apply(clazz: Class[_], programArgs: Array[String] = Array.empty, javaArgs: Array[String] = Array.empty): JavaLauncher = apply(clazz.getName, programArgs, javaArgs)

  def parseArgs(args: Array[String]): (String, Array[String], Array[String]) = {
    if (args.length == 1)
      (args.head, Array.empty[String], Array.empty[String])
    else if (!Launcher.isInt(args(1))) {
      val programArguments = args.drop(1)
      (args.head, programArguments, Array.empty[String])
    }
    else {
      val programArgumentCount = args(1).toInt
      val programArguments = args.drop(2).take(programArgumentCount)
      val javaArguments = args.drop(2 + programArgumentCount)
      (args.head, programArguments, javaArguments)
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      val syntax = s"""
        |Syntax: ${this.getClass.getSimpleName.dropRight(1)}
        |  mainClass nonIntProgramArg otherProgramArgs*
        |  mainClass intProgramArgCount programArguments{int} javaArgs*
        |""".stripMargin
      println(syntax)
    }
    else {
      val (classname, programArgs, javaArgs) = parseArgs(args)
      val launcher = JavaLauncher(classname, programArgs, javaArgs)

      launcher.launch(true)
    }
  }
}

class SbtLauncher(projectNameOpt: Option[String], classname: String, programArgs: Array[String] = Array.empty, javaArgs: Array[String], sbtArgs: Array[String]) extends Launcher(SbtLauncher.mkArgs(projectNameOpt, classname, programArgs, javaArgs, sbtArgs)) {

  override def newProcessBuilder(): ProcessBuilder = {
    val processBuilder = super.newProcessBuilder()
    val javaArgsString = ("-Dfile.encoding=UTF-8" +: javaArgs).mkString(" ")
    processBuilder.environment.put("_JAVA_OPTIONS", javaArgsString)
    processBuilder
  }
}

object SbtLauncher {

  protected def isWindows(): Boolean = {
    System.getProperty("os.name").toLowerCase().contains("win")
  }

  protected def mkArgs(projectNameOpt: Option[String], classname: String, programArgs: Array[String], javaArgs: Array[String], sbtArgs: Array[String]): Array[String] = {
    val arrayBuilder = ArrayBuilder.make[String]
    val sbtString = if (isWindows()) "sbt.bat" else "sbt"
    val task = projectNameOpt.map(_ + "/runMain").getOrElse("runMain")
    val programArgsString = if (programArgs.isEmpty) "" else programArgs.mkString(" ", " ", "")

    arrayBuilder += sbtString
    arrayBuilder ++= sbtArgs
//    arrayBuilder += "-Dfile.encoding=UTF-8"
//    arrayBuilder ++= javaArgs.map { javaArg => s"-J$javaArg" }
    arrayBuilder += s""""$task $classname$programArgsString""""
    arrayBuilder.result
  }

  def apply(projectNameOpt: Option[String], classname: String, programArgs: Array[String], javaArgs: Array[String], sbtArgs: Array[String]): SbtLauncher = new SbtLauncher(projectNameOpt, classname, programArgs, javaArgs, sbtArgs)

  def apply(classname: String, programArgs: Array[String], javaArgs: Array[String], sbtArgs: Array[String]): SbtLauncher = new SbtLauncher(None, classname, programArgs, javaArgs, sbtArgs)

  def apply(clazz: Class[_], programArgs: Array[String] = Array.empty, javaArgs: Array[String] = Array.empty, sbtArgs: Array[String] = Array.empty): SbtLauncher = apply(clazz.getName, programArgs, javaArgs, sbtArgs)

  def parseArgs(args: Array[String]): (String, Array[String], Array[String], Array[String]) = {
    if (args.length == 1)
      (args.head, Array.empty[String], Array.empty[String], Array.empty[String])
    else if (!Launcher.isInt(args(1))) {
      val programArgs = args.drop(1)
      (args.head, programArgs, Array.empty[String], Array.empty[String])
    }
    else {
      val programArgCount = args(1).toInt
      val programArgs = args.drop(1 + 1).take(programArgCount)

      if (!args.isDefinedAt(1 + 1 + programArgCount))
        (args.head, programArgs, Array.empty[String], Array.empty[String])
      else if (!Launcher.isInt(args(1 + 1 + programArgCount))) {
        val javaArgs = args.drop(1 + 1 + programArgCount)
        (args.head, programArgs, javaArgs, Array.empty[String])
      }
      else {
        val javaArgCount = args(1 + 1 + programArgCount).toInt
        val javaArgs = args.drop(1 + 1 + programArgCount + 1).take(javaArgCount)
        val sbtArgs = args.drop(1 + 1 + programArgCount + 1 + javaArgCount)
        (args.head, programArgs, javaArgs, sbtArgs)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      val syntax = s"""
        |Syntax: ${this.getClass.getSimpleName.dropRight(1)}
        |  mainClass
        |  mainClass nonIntProgramArg otherProgramArgs*
        |  mainClass intProgramArgCount programArgs{intProgramArgCount} [nonIntJavaArg otherJavaArgs*]
        |  mainClass intProgramArgCount programArgs{intProgramArgCount} [intJavaArgCount javaArgs{intJavaArgCount} sbtArgs*]
        |""".stripMargin
      println(syntax)
    }
    else {
      val (classname, programArgs, javaArgs, sbtArgs) = parseArgs(args)
      val launcher = SbtLauncher(classname, programArgs, javaArgs, sbtArgs)

      launcher.launch(true)
    }
  }
}
