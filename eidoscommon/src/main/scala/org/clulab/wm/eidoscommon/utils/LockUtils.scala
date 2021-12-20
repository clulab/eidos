package org.clulab.wm.eidoscommon.utils

import java.io.File

object LockUtils {

  def findFiles(dir: String, dataExt: String, lockExt: String): Seq[File] = {
    val dataFiles = FileUtils.findFiles(dir, dataExt)
    val unlockedDataFiles = dataFiles.filter { dataFile =>
      val lockFile = FileEditor(dataFile).setExt(lockExt).get
      !lockFile.exists
    }

    unlockedDataFiles
  }

  // Remove any lock files have become extraneous in that there is no
  // corresponding data file, probably because it has been processed.
  def cleanupLocks(dir: String, lockExt: String, dataExt: String): Unit = {
    val lockFiles = FileUtils.findFiles(dir, lockExt)

    lockFiles.foreach { lockFile =>
      val dataFile = FileEditor(lockFile).setExt(dataExt).get
      if (!dataFile.exists)
        lockFile.delete()
    }
  }

  def withLock[T](file: File, lockExt: String)(f: => T): T = {
    val lockFile = FileEditor(file).setExt(lockExt).get

    try {
      lockFile.createNewFile()
      f
    }
    finally {
      lockFile.delete()
    }
  }
}
