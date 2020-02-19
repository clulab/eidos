package ai.lum.eidos.sparql.utils

import java.io._
import java.net.URL
import java.nio.file.Paths

import ai.lum.eidos.sparql.utils.Closer.AutoCloser

import scala.io.Source

object FileUtils {

  def appendingPrintWriterFromFile(file: File): PrintWriter = Sinker.printWriterFromFile(file, append = true)

  def appendingPrintWriterFromFile(path: String): PrintWriter = Sinker.printWriterFromFile(path, append = true)

  def printWriterFromFile(file: File): PrintWriter = Sinker.printWriterFromFile(file, append = false)

  def printWriterFromFile(path: String): PrintWriter = Sinker.printWriterFromFile(path, append = false)

  def findFiles(collectionDir: String, extension: String): Seq[File] = {
    val dir = new File(collectionDir)
    val filter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name.endsWith(extension)
    }

    val result = Option(dir.listFiles(filter))
        .getOrElse(throw Sourcer.newFileNotFoundException(collectionDir))
    result
  }

  def getCommentedLinesFromSource(source: Source): Iterator[String] =
      source
          .getLines()
          // Skips "empty" lines as well as comments
          .filter(line => !line.startsWith("#") && line.trim().nonEmpty)

  // Add FromFile as necessary.  See getText below.
  def getCommentedTextSetFromResource(path: String): Set[String] =
      Sourcer.sourceFromResource(path).autoClose { source =>
        getCommentedLinesFromSource(source).map(_.trim).toSet
      }

  // Add FromResource as necessary.  See getText below,
  def getCommentedTextFromFile(file: File, sep: String = " "): String =
      Sourcer.sourceFromFile(file).autoClose { source =>
        // These haven't been trimmed in case esp. trailing spaces are important.
        getCommentedLinesFromSource(source).mkString(sep)
      }

  protected def getTextFromSource(source: Source): String = source.mkString

  def getTextFromResource(path: String): String =
      Sourcer.sourceFromResource(path).autoClose { source =>
        getTextFromSource(source)
      }

  def getTextFromFile(file: File): String =
      Sourcer.sourceFromFile(file).autoClose { source =>
        getTextFromSource(source)
      }

  def getTextFromFile(path: String): String =
      Sourcer.sourceFromFile(path).autoClose { source =>
        getTextFromSource(source)
      }

  def copyResourceToFile(src: String, dest: File): Unit = {
    FileUtils.getClass.getResourceAsStream(src).autoClose { is: InputStream =>
      new FileOutputStream(dest).autoClose { os: FileOutputStream =>
        val buf = new Array[Byte](8192)

        def transfer: Boolean = {
          val len = is.read(buf)
          val continue =
            if (len > 0) {
              os.write(buf, 0, len); true
            }
            else false

          continue
        }

        while (transfer) { }
      }
    }
  }

  def withResourceAsFile[T](resourcePath: String)(function: File => T): T = {
    val resource: URL = Option(this.getClass.getResource(resourcePath))
        .getOrElse(throw new IOException("Resource " + resourcePath + " could not be found."))
    val (file, temporary) =
        if (resource.getProtocol == "file")
        // See https://stackoverflow.com/questions/6164448/convert-url-to-normal-windows-filename-java/17870390
          (Paths.get(resource.toURI).toFile, false)
        else {
          // If a single file is to be (re)used, then some careful synchronization needs to take place.
          // val tmpFile = new File(cacheDir + "/" + StringUtils.afterLast(timeNormModelPath, '/') + ".tmp")
          // Instead, make a new temporary file each time and delete it afterwards.
          val tmpFile = File.createTempFile(
            StringUtils.afterLast(resourcePath, '/') + '-', // Help identify the file later.
            "." + StringUtils.afterLast(resourcePath, '.') // Keep extension for good measure.
          )

          try {
            FileUtils.copyResourceToFile(resourcePath, tmpFile)
            (tmpFile, true)
          }
          catch {
            case exception: Throwable =>
              tmpFile.delete()
              throw exception
          }
        }

    try {
      function(file)
    }
    finally {
      if (temporary)
        file.delete()
    }
  }

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
}
