package org.clulab.wm.eidos.apps.cache

import ai.lum.common.ConfigUtils._
import org.clulab.geonorm.GeoNamesIndex
import org.clulab.wm.eidos.EidosSystem

import java.nio.file.Path
import java.nio.file.Paths

object CacheGeonames extends App {
  val config = EidosSystem.defaultConfig
  val geoNamesDir: Path = Paths.get(config[String]("geonorm.geoNamesDir")).toAbsolutePath.normalize

  GeoNamesIndex.fromClasspathJar(geoNamesDir)
}
