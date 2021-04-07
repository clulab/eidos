package org.clulab.wm.wmexchanger.utils

import org.clulab.wm.eidoscommon.utils.FileEditor
import org.clulab.wm.eidoscommon.utils.FileUtils

object LockUtils {

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
