package org.clulab.wm.wmexchanger.wmeidos

import org.clulab.wm.eidos.utils.SbtLauncher

object EidosLoopAppLauncher extends App {

  val (project, programArgs, javaArgs, sbtArgs) = SbtLauncher.parseArgs(args)

  val launcher = SbtLauncher(Some(project), classOf[EidosLoopApp].getName, programArgs, javaArgs, sbtArgs)

  launcher.launch()
}
