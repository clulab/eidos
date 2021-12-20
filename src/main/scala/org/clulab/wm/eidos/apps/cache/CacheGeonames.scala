package org.clulab.wm.eidos.apps.cache

import ai.lum.common.ConfigUtils._
import org.clulab.geonorm.GeoNamesIndex
import org.clulab.wm.eidos.EidosSystem

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

object CacheGeonames extends App {
  val config = EidosSystem.defaultConfig
  val geoNamesDir: Path = Paths.get(config[String]("geonorm.geoNamesDir")).toAbsolutePath.normalize

  def deleteFiles(path: Path): Unit = {
    if (Files.exists(path)) {
      // This is written for Scala 1.11 compatibility.
      Files.list(path).toArray.foreach { path =>
        Files.deleteIfExists(path.asInstanceOf[Path])
      }
      // At least on Windows, the directory doesn't disappear until the application exists.
      // In the meantime, it will be locked and unavailable for use by GeoNamesIndex.
      // However, it is is left around but empty, it works OK.  So, skip this.
      // Files.deleteIfExists(path)
    }
  }

  deleteFiles(geoNamesDir)
  GeoNamesIndex.fromClasspathJar(geoNamesDir)
}
