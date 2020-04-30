package org.clulab.wm.utils

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.FilenameFilter
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.PrintWriter

import org.clulab.wm.utils.Closer.AutoCloser

import scala.io.Source

object FileUtils {

  def appendingPrintWriterFromFile(file: File): PrintWriter = Sinker.printWriterFromFile(file, append = true)

  def appendingPrintWriterFromFile(path: String): PrintWriter = Sinker.printWriterFromFile(path, append = true)

  def printWriterFromFile(file: File): PrintWriter = Sinker.printWriterFromFile(file, append = false)

  def printWriterFromFile(path: String): PrintWriter = Sinker.printWriterFromFile(path, append = false)

  // Output
  def newBufferedOutputStream(file: File): BufferedOutputStream =
    new BufferedOutputStream(new FileOutputStream(file))

  def newBufferedOutputStream(filename: String): BufferedOutputStream =
    newBufferedOutputStream(new File(filename))

  def newAppendingBufferedOutputStream(file: File): BufferedOutputStream =
    new BufferedOutputStream(new FileOutputStream(file, true))

  def newAppendingBufferedOutputStream(filename: String): BufferedOutputStream =
    newAppendingBufferedOutputStream(new File(filename))

  def newObjectOutputStream(filename: String): ObjectOutputStream =
    new ObjectOutputStream(newBufferedOutputStream(filename))

  // Input
  def newBufferedInputStream(file: File): BufferedInputStream =
    new BufferedInputStream(new FileInputStream(file))

  def newBufferedInputStream(filename: String): BufferedInputStream =
    newBufferedInputStream(new File(filename))

  def newObjectInputStream(filename: String): ObjectInputStream =
    new ObjectInputStream(newBufferedInputStream(filename))

  def findFiles(collectionDir: String, extension: String): Seq[File] = {
    val dir = new File(collectionDir)
    val filter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name.endsWith(extension)
    }

    val result = Option(dir.listFiles(filter))
        .getOrElse(throw Sourcer.newFileNotFoundException(collectionDir))
    result
  }

  protected def getTextFromSource(source: Source): String = source.mkString

  def getTextFromFile(file: File): String =
    Sourcer.sourceFromFile(file).autoClose { source =>
      getTextFromSource(source)
    }
}
