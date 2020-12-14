package org.clulab.wm.eidos.apps

import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config
import org.clulab.odin.Mention
import org.clulab.odin.State
import org.clulab.processors.Document
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.context.GeoNormFinder

object RunDynetGeoNorm extends App {

  def fromConfig(config: Config): GeoNormFinder = {
    // val model = config[String]("model")
    // Do metal stuff.

    // Eventually the code should be just
    GeoNormFinder.fromConfig(config)
    // but for now, it is this:
    // null
  }

  def find(doc: Document, initialState: State): Seq[Mention] = {
    // Eventually the code should be just
    dynetGeoNormFinder.find(doc, initialState)
    // but for now, it is this:
    // Seq.empty
  }
  val text = "West Indian all-rounder Phil Simmons took four for 38 on Friday"
  //val text = "Early rainfall in Ethiopia improves harvests tremendously." // args(0)
  val config = EidosSystem.defaultConfig
  val dynetGeoNormFinder = fromConfig(config[Config]("geonorm"))
  val eidosSystem = new EidosSystem(config)
  val doc = eidosSystem.annotate(text)
  val mentions = eidosSystem.extractMentionsFrom(doc)

  // val state = new State()

  // find(doc, state)
}
