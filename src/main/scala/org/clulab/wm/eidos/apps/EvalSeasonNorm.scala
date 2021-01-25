package org.clulab.wm.eidos.apps

import com.typesafe.config.ConfigValueFactory
import org.clulab.wm.eidos.EidosSystem

import scala.collection.JavaConverters._


object EvalSeasonNorm {

  protected def run(): Double = {
    // Configure eidos to apply SeasonFinder
    val config = EidosSystem.defaultConfig
    val newConfig = config
      .withValue("EidosSystem.finders", ConfigValueFactory.fromAnyRef(List("timenorm", "geonorm" ,"seasons").asJava))
      .withValue("ontologies.useGrounding", ConfigValueFactory.fromAnyRef(false))
    val useNeuralParser: Boolean = newConfig.getBoolean("timenorm.useNeuralParser")
    val eidosSystem = new EidosSystem(newConfig)

    EvalTimeNorm.runEval(eidosSystem, "SeasonFinder",
      useNeuralParser, "Ethiopia_Food_Security_Outlook_1-Feb-17.csv")
  }

  def test(): Double = run()

  def main(args: Array[String]): Unit = run()
}
