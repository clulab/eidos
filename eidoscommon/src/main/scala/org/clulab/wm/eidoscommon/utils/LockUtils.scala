package org.clulab.wm.eidoscommon.utils

import java.io.File

object LockUtils {

  def findFiles(dir: String, dataExt: String, lockExt: String): Seq[File] =
      findFiles(dir, Seq(dataExt), lockExt)

  def findFiles(dir: String, dataExts: Seq[String], lockExt: String): Seq[File] = {
    val allFiles = FileUtils.findFiles(dir, dataExts :+ lockExt)
    val (lockFilesSeq, dataFiles) = allFiles.partition { file => file.getName.endsWith(lockExt) }
    val lockFilesSet = lockFilesSeq.map(_.getName).toSet
    val unlockedDataFiles = dataFiles.filter { dataFile =>
      val lockFile = FileEditor(dataFile).setExt(lockExt, last = false).get.getName

      !lockFilesSet(lockFile)
    }

    unlockedDataFiles
  }

  def hasLock(dataFile: File, lockExt: String): Boolean = {
    val lockFile = FileEditor(dataFile).setExt(lockExt, last = false).get

    lockFile.exists
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
