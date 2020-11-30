package org.clulab.wm.eidoscommon.utils

import java.io._
import java.nio.charset.StandardCharsets

import org.clulab.wm.eidoscommon.utils.Closer.AutoCloser

class TestDiskFull extends Test {

  def test1 = {
    val file = "/E:/full.dat"
    var i = 0

    try {
      val text1 = "The quick brown fox jumped over the lazy dog."
      val text = text1 + text1

      for (limit <- 1 until 400) {
        val fos = new FileOutputStream(file)
        val osw = new OutputStreamWriter(new BufferedOutputStream(fos), StandardCharsets.UTF_8.toString)
        i = 0

        new PrintWriter(osw).autoClose { pw =>
          while (i < limit) {
            pw.print(text)
            i += 1
            //          pw.flush()
            //          osw.flush()
            //          fos.flush()
            fos.getFD.sync()
          }
        }
      }
    }
    catch {
      case exception: SyncFailedException =>
        println(s"Synchronization failed for file $file at $i")
        println("Exiting with code -2 on assumption that the disk is full")
        System.exit(-2)
      case exception: IOException =>
        println(s"IO failed for file $file at $i")
        println("Exiting with code -2 on assumption that the disk is full")
        System.exit(-2)
      case exception: Exception =>
        println(s"Exception for file $file at $i")
        exception.printStackTrace()
      case throwable: Throwable =>
        println(s"Throwable for file $file at $i")
        throwable.printStackTrace()
    }
  }

//  test1
}
