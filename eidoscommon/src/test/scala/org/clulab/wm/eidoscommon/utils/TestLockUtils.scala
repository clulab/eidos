package org.clulab.wm.eidoscommon.utils

import java.io.File

class TestLockUtils extends Test {
  
  behavior of "File locking mechanism"

  val dataExt = "jsonld"
  val lockExt = "lock"

  it should "work with dangling lock files" in {
    val lockFile = new File("dangling.lock")

    lockFile.createNewFile()
    lockFile.exists should be (true)

    lockFile.createNewFile()
    lockFile.exists should be (true)

    lockFile.delete()
  }

  it should "work with missing lock files" in {
    val lockFile = new File("missing.lock")

    lockFile.delete()
    lockFile.delete()
  }
}
