package org.clulab.wm.wmexchanger.utils

import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils
import org.clulab.wm.eidoscommon.utils.Sourcer

import java.io.File
import java.io.FilenameFilter

object LockUtils {

  def findFiles(dir: String, dataExt: String, lockExt: String): Seq[File] = {
    val dataFiles = FileUtils.findFiles(dir, dataExt)
    val lockableDataFiles = dataFiles.filter { dataFile =>
      val lockFile = FileEditor(dataFile).setExt(lockExt).get
      lockFile.exists
    }

    lockableDataFiles
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
}
