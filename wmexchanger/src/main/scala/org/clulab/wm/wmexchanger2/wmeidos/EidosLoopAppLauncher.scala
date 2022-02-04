package org.clulab.wm.wmexchanger2.wmeidos

import org.clulab.wm.eidos.utils.SbtLauncher

object EidosLoopAppLauncher extends App {

  val (project, programArgs, javaArgs, sbtArgs) = SbtLauncher.parseArgs(args)

  val launcher = SbtLauncher(Some(project), classOf[EidosLoopApp2].getName, programArgs, javaArgs, sbtArgs)

  launcher.launch()
}
