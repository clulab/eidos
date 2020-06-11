package org.clulab.wm.eidos.apps

import java.nio.file.Path
import java.nio.file.Paths

import ai.lum.common.ConfigUtils._
import com.typesafe.config.ConfigFactory
import org.clulab.geonorm.GeoNamesIndex
import org.clulab.wm.eidos.EidosSystem

object CacheGeonames extends App {
  val config = ConfigFactory.load(EidosSystem.defaultConfig)
  val geoNamesDir: Path = Paths.get(config[String]("geonorm.geoNamesDir")).toAbsolutePath.normalize

  GeoNamesIndex.fromClasspathJar(geoNamesDir)
}
