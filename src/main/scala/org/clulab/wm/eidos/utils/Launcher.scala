package org.clulab.wm.eidos.utils

import org.clulab.wm.eidoscommon.utils.Counter
import org.clulab.wm.eidoscommon.utils.Holder

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuilder

class Launcher(val args: Array[String]) {
  protected val counter: Counter = Counter()

  def getCount: Int = counter.get

  def newProcessBuilder(args: Array[String]): ProcessBuilder = {
    val processBuilder = new ProcessBuilder(args.toList.asJava)

    processBuilder.redirectInput(ProcessBuilder.Redirect.INHERIT)
    processBuilder.redirectOutput(ProcessBuilder.Redirect.INHERIT)
    processBuilder.redirectError(ProcessBuilder.Redirect.INHERIT)
    processBuilder
  }

  def launch(args: Array[String]): Int = {
    val processBuilder = newProcessBuilder(args)
    val process = processBuilder.start()
    process.waitFor()
  }

  // If retry is true, runs the program until it returns 0.
  // If it is false, runs it just until it returns.
  // Either way, it returns the result of the program.
  def launch(retry: Boolean = true): Int = {
    val resultHolder = new Holder[Int](0)

    while ({
      counter.inc()
      resultHolder.set(launch(args)) != 0 && retry
    }) { }
    resultHolder.get
  }
}

object Launcher {

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

  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      val syntax =
        s"""
           |Syntax: ${this.getClass.getSimpleName.dropRight(1)}

           |

           |  mainClass nonIntProgramArgument otherProgramA

           |  mainClass intProgramArgumentCount programArguments{int} javaA

           |""".stripMargin
      println(syntax)
    }
    else {
      val launcher =  if (args.length == 1)
        JavaLauncher(args.head, Array.empty[String], Array.empty[String])
      else if (!Launcher.isInt(args(1))) {
        val programArguments = args.drop(1)
        JavaLauncher(args.head, programArguments, Array.empty[String])
      }
      else {
        val programArgumentCount = args(1).toInt
        val programArguments = args.drop(2).take(programArgumentCount)
        val javaArguments = args.drop(2 + programArgumentCount)
        JavaLauncher(args.head, programArguments, javaArguments)
      }
      launcher.launch(true)
    }
  }
}

class SbtLauncher(classname: String, programArgs: Array[String] = Array.empty, javaArgs: Array[String], sbtArgs: Array[String]) extends Launcher(SbtLauncher.mkArgs(classname, programArgs, javaArgs, sbtArgs))

object SbtLauncher {

  protected def isWindows(): Boolean = {
    System.getProperty("os.name").toLowerCase().contains("win")
  }
  protected def mkArgs(classname: String, programArgs: Array[String], javaArgs: Array[String], sbtArgs: Array[String]): Array[String] = {
    val arrayBuilder = ArrayBuilder.make[String]

    arrayBuilder += (if (isWindows()) "sbt.bat" else "sbt")
    arrayBuilder ++= sbtArgs
    arrayBuilder += "-J-Dfile.encoding=UTF-8"
    arrayBuilder ++= javaArgs.map { javaArg => s"-J$javaArg" }
    arrayBuilder += s""""runMain $classname ${programArgs.mkString(" ")}""""
    arrayBuilder.result
  }

  def apply(classname: String, programArgs: Array[String], javaArgs: Array[String], sbtArgs: Array[String]): SbtLauncher = new SbtLauncher(classname, programArgs, javaArgs, sbtArgs)

  def apply(clazz: Class[_], programArgs: Array[String] = Array.empty, javaArgs: Array[String] = Array.empty, sbtArgs: Array[String] = Array.empty): SbtLauncher = apply(clazz.getName, programArgs, javaArgs, sbtArgs)

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
      val launcher = if (args.length == 1)
        SbtLauncher(args.head, Array.empty[String], Array.empty[String], Array.empty[String])
      else if (!Launcher.isInt(args(1))) {
        val programArgs = args.drop(1)
        SbtLauncher(args.head, programArgs, Array.empty[String], Array.empty[String])
      }
      else {
        val programArgCount = args(1).toInt
        val programArgs = args.drop(1 + 1).take(programArgCount)

        if (!args.isDefinedAt(1 + 1 + programArgCount))
          SbtLauncher(args.head, programArgs, Array.empty[String], Array.empty[String])
        else if (!Launcher.isInt(args(1 + 1 + programArgCount))) {
          val javaArgs = args.drop(1 + 1 + programArgCount)
          SbtLauncher(args.head, programArgs, javaArgs, Array.empty[String])
        }
        else {
          val javaArgCount = args(1 + 1 + programArgCount).toInt
          val javaArgs = args.drop(1 + 1 + programArgCount + 1).take(javaArgCount)
          val sbtArgs = args.drop(1 + 1 + programArgCount + 1 + javaArgCount)
          SbtLauncher(args.head, programArgs, javaArgs, sbtArgs)
        }
      }
      launcher.launch(true)
    }
  }
}
