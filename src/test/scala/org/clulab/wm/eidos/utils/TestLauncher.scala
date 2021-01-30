package org.clulab.wm.eidos.utils

import org.clulab.wm.eidos.test.EidosTest

class TestLauncher extends EidosTest {

  class DestroyingJavaLauncher(force: Boolean, classname: String, args: Array[String]) extends JavaLauncher(classname, args) {

    override def launch(args: Array[String]): Int = {
      val processBuilder = newProcessBuilder(args)
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

  behavior of "Launcher"

  ignore should "launch ExitNormally once" in {
    val launcher = JavaLauncher(classOf[ExitNormally], Array("one", "two", "three"))

    launcher.launch()
    launcher.getCount should be (1)
  }

  ignore should "launch ExitAbnormally twice" in {
    val lockFile = ExitAbnormally.lockFile
    lockFile.createNewFile()

    val launcher = JavaLauncher(classOf[ExitAbnormally], Array("one", "two", "three"))
    launcher.launch()
    launcher.getCount should be (2)
  }

  ignore should "launch ExitWithSignal without force twice" in {
    val lockFile = ExitWithSignal.lockFile
    lockFile.createNewFile()

    val launcher = DestroyingJavaLauncher(force = false, classOf[ExitWithSignal], Array("one", "two", "three"))
    launcher.launch()
    launcher.getCount should be (2)
  }

  ignore should "launch ExitWithSignal with force twice" in {
    val lockFile = ExitWithSignal.lockFile
    lockFile.createNewFile()

    val launcher = DestroyingJavaLauncher(force = true, classOf[ExitWithSignal], Array("one", "two", "three"))
    launcher.launch()
    launcher.getCount should be (2)
  }

  it should "launch ExitOnHeapExhaustion twice" in {
    val lockFile = ExitOnHeapExhaustion.lockFile
    lockFile.createNewFile()

    val launcher = JavaLauncher(classOf[ExitOnHeapExhaustion], Array("one", "two", "three"))
    launcher.launch()
    launcher.getCount should be (2)
  }
}
