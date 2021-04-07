package org.clulab.wm.wmexchanger.utils

import org.clulab.wm.eidoscommon.utils.{Logging, Sinker}

import java.io.{File, FileOutputStream, FileWriter}
import java.nio.channels.FileChannel
import java.util.Scanner


object OpenFile extends App with Logging {
  println(s"Opening ${args(0)}")
  val file = new File(args(0))

  val printWriter = Sinker.printWriterFromFile(file, append = false)
  val fileWriter = new FileWriter(file)

  val fileOutputStream = new FileOutputStream(file)
  val channel: FileChannel = fileOutputStream.getChannel
  val fileLock = channel.lock()

  fileOutputStream.write(100)

  println("Press ENTER to exit...")
  new Scanner(System.in).nextLine()
  println("User interruption")

  fileOutputStream.write(100)
  printWriter.close()
  println("Done!")
}
