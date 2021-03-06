package org.clulab.wm.eidos.utils

import org.clulab.wm.eidos.test.EidosTest

import java.io.File

class TestLauncher extends EidosTest {

  val javaIt = it
  val sbtIt = ignore

  // See https://stackoverflow.com/questions/32460295/how-to-programmatically-ignore-skip-tests-with-scalatest.
  // These tests do not work from within sbt because the classpath cannot be automatically calculated.
  // Use this flag to cancel them.  The tests can be run manually from IntelliJ, for example.
  val isNotSbt = {
    val classpath = System.getProperty("java.class.path")
    !classpath.endsWith("sbt-launch.jar")
  }

  class DestroyingJavaLauncher(force: Boolean, classname: String, args: Array[String]) extends JavaLauncher(classname, args, Array.empty) {

    override def launchOnce(): Int = {
      val processBuilder = newProcessBuilder()
      val process = processBuilder.start()

      // Give the program enough time to start up.
      Thread.sleep(5000)
      if (force)
        process.destroyForcibly()
      else
        process.destroy()
      process.waitFor()
    }
  }

  object DestroyingJavaLauncher {

    def apply(force: Boolean, classname: String, args: Array[String]): DestroyingJavaLauncher = new DestroyingJavaLauncher(force, classname, args)

    def apply(force: Boolean, clazz: Class[_], args: Array[String] = Array.empty): DestroyingJavaLauncher = apply(force, clazz.getName, args)
  }

  behavior of "JavaLauncher"

  javaIt should "launch ExitNormally once" in {
    assume(isNotSbt)
    val launcher = JavaLauncher(classOf[ExitNormally], Array("one", "two", "three"))

    launcher.launch()
    launcher.getCount should be (1)
  }

  def testJavaLaunchTwice(lockFile: File, clazz: Class[_]): Unit = {
    lockFile.createNewFile()

    val launcher = JavaLauncher(clazz, Array("one", "two", "three"))
    launcher.launch()
    launcher.getCount should be (2)
  }

  javaIt should "launch ExitAbnormally twice" in {
    assume(isNotSbt)
    testJavaLaunchTwice(ExitAbnormally.lockFile, classOf[ExitAbnormally])
  }

  def testExitOnDestroy(force: Boolean): Unit = {
    val lockFile = ExitOnDestroy.lockFile
    lockFile.createNewFile()

    val launcher = DestroyingJavaLauncher(force = force, classOf[ExitOnDestroy], Array("one", "two", "three"))
    launcher.launch()
    launcher.getCount should be (2)
  }

  javaIt should "launch ExitOnDestroy without force twice" in {
    assume(isNotSbt)
    testExitOnDestroy(false)
  }

  javaIt should "launch ExitOnDestroy with force twice" in {
    assume(isNotSbt)
    testExitOnDestroy(true)
  }

  javaIt should "launch ExitOnHeapExhaustion twice" in {
    assume(isNotSbt)
    testJavaLaunchTwice(ExitOnHeapExhaustion.lockFile, classOf[ExitOnHeapExhaustion])
  }

  javaIt should "launch ExitOnCrash twice" in {
    assume(isNotSbt)
    testJavaLaunchTwice(ExitOnCrash.lockFile, classOf[ExitOnCrash])
  }

  javaIt should "launch ExitOnSignal twice" in {
    assume(isNotSbt)
    testJavaLaunchTwice(ExitOnSignal.lockFile, classOf[ExitOnSignal])
  }

  behavior of "SbtLauncher"

  sbtIt should "launch ExitNormally once" in {
    val launcher = SbtLauncher(classOf[ExitNormally], Array("one", "two", "three"))

    launcher.launch()
    launcher.getCount should be (1)
  }
}
