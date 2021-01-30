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
}

class JavaLauncher(classname: String, args: Array[String]) extends Launcher(JavaLauncher.mkArgs(classname, args))

object JavaLauncher {

  protected def mkArgs(classname: String, args: Array[String]): Array[String] = {
    // Also configure garbage collector for best type
    // and any settings it needs.
    val arrayBuilder = ArrayBuilder.make[String]

    arrayBuilder += "java"
    arrayBuilder += "-classpath"
    arrayBuilder += System.getProperty("java.class.path")
    arrayBuilder += classname
    arrayBuilder ++= args
    arrayBuilder.result
  }

  def apply(classname: String, args: Array[String]): JavaLauncher = new JavaLauncher(classname, args)

  def apply(clazz: Class[_], args: Array[String] = Array.empty): JavaLauncher = apply(clazz.getName, args)
}

class SbtLauncher(classname: String, args: Array[String] = Array.empty) extends Launcher(args) {

}