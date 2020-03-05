package org.clulab.wm.eidos.utils

import java.io._
import java.net.URL
import java.nio.file.StandardCopyOption
import java.nio.file.{Files, Path, Paths}
import java.util.Collection
import java.util.zip.ZipFile

import org.clulab.serialization.json.stringify
import org.clulab.utils.ClassLoaderObjectInputStream
import org.clulab.wm.eidos.EidosSystem
import org.clulab.wm.eidos.document.AnnotatedDocument
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.clulab.wm.eidos.utils.Closer.AutoCloser
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.collection.JavaConverters._
import scala.io.Source

object FileUtils {

  def appendingPrintWriterFromFile(file: File): PrintWriter = Sinker.printWriterFromFile(file, append = true)

  def appendingPrintWriterFromFile(path: String): PrintWriter = Sinker.printWriterFromFile(path, append = true)

  def printWriterFromFile(file: File): PrintWriter = Sinker.printWriterFromFile(file, append = false)

  def printWriterFromFile(path: String): PrintWriter = Sinker.printWriterFromFile(path, append = false)

  //
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

  def loadYamlFromResource(path: String): Collection[Any] = {
    val input = getTextFromResource(path)
    val yaml = new Yaml(new Constructor(classOf[Collection[Any]]))

    yaml.load(input).asInstanceOf[Collection[Any]]
  }

  def writeToJSONLD(annotatedDocument: AnnotatedDocument, pw: PrintWriter, reader: EidosSystem): Unit = {
    // 4. Convert to JSON
    val corpus = new JLDCorpus(annotatedDocument)
    val mentionsJSONLD = corpus.serialize()
    // 5. Write to output file
    pw.println(stringify(mentionsJSONLD, pretty = true))
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

  def newClassLoaderObjectInputStream(filename: String, classProvider: Any = this): ClassLoaderObjectInputStream = {
    val classLoader = classProvider.getClass.getClassLoader

    new ClassLoaderObjectInputStream(classLoader, new FileInputStream(filename))
  }

  def load[A](filename: String, classProvider: Any): A =
      newClassLoaderObjectInputStream(filename, classProvider).autoClose { objectInputStream =>
        objectInputStream.readObject().asInstanceOf[A]
      }

  def load[A](bytes: Array[Byte], classProvider: Any): A = {
    val classLoader = classProvider.getClass.getClassLoader

    new ClassLoaderObjectInputStream(classLoader, new ByteArrayInputStream(bytes)).autoClose { objectInputStream =>
      objectInputStream.readObject().asInstanceOf[A]
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
            getName(resourcePath) + '-', // Help identify the file later.
            getExt(resourcePath) // Keep extension for good measure.
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

  def unzip(zipPath: Path, outputPath: Path, replace: Boolean = false): Unit = {
    new ZipFile(zipPath.toFile).autoClose { zipFile =>
      for (entry <- zipFile.entries.asScala) {
        val path = outputPath.resolve(entry.getName)
        if (entry.isDirectory) {
          Files.createDirectories(path)
        } else {
          Files.createDirectories(path.getParent)
          if (replace)
            Files.copy(zipFile.getInputStream(entry), path, StandardCopyOption.REPLACE_EXISTING)
          else
            Files.copy(zipFile.getInputStream(entry), path)
        }
      }
    }
  }

  protected def replaceNameExtension(file: File, newExtension: String): String = {
    StringUtils.beforeLast(file.getName, '.') + newExtension
  }

  protected def getName(filename: String): String = {
    StringUtils.afterLast(filename, '/')
  }

  protected def getExt(filename: String): String = {
    "." + StringUtils.afterLast(filename, '.')
  }
}
