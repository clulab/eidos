package org.clulab.wm.eidos.utils

import java.io._
import java.util.Collection

import org.clulab.serialization.json.stringify
import org.clulab.utils.ClassLoaderObjectInputStream
import org.clulab.wm.eidos.{AnnotatedDocument, EidosSystem}
import org.clulab.wm.eidos.serialization.json.JLDCorpus
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.io.Source

object FileUtils {

//  type Closing = { def close(): Unit }
//
//  implicit class AutoClose(arg: Closing) {
//    def autoClose[A <: Closing, B](f: A => B): B = {
//      try {
//        f(arg)
//      }
//      finally {
//        Option(arg).map(_.close())
//      }
//    }
//  }

  def autoClose[A <: { def close(): Unit }, B](resource: A)(f: A => B): B = {
    try {
      f(resource)
    }
    finally {
      Option(resource).map(_.close())
    }
  }

  def printWriterFromFile(file: File): PrintWriter = Sinker.printWriterFromFile(file)

  def printWriterFromFile(path: String): PrintWriter = Sinker.printWriterFromFile(path)

  def findFiles(collectionDir: String, extension: String): Seq[File] = {
    val dir = new File(collectionDir)
    val filter = new FilenameFilter {
      def accept(dir: File, name: String): Boolean = name.endsWith(extension)
    }

    val result = dir.listFiles(filter)
    if (result == null)
      throw Sourcer.newFileNotFoundException(collectionDir)
    result
  }

  def getCommentedLinesFromSource(source: Source): Array[String] =
      source
          .getLines()
          .toArray
          // Skips "empty" lines as well as comments
          .filter(line => !line.startsWith("#") && line.trim().nonEmpty)

  // Add FromFile as necessary.  See getText below.
  def getCommentedTextsFromResource(path: String): Seq[String] =
      autoClose(Sourcer.sourceFromResource(path)) { source =>
        getCommentedLinesFromSource(source).map(_.trim)
      }

  // Add FromResource as necessary.  See getText below,
  def getCommentedTextFromFile(file: File, sep: String = " "): String =
      autoClose(Sourcer.sourceFromFile(file)) { source =>
        // These haven't been trimmed in case esp. trailing spaces are important.
        getCommentedLinesFromSource(source).mkString(sep)
      }

  protected def getTextFromSource(source: Source): String = source.mkString

  def getTextFromResource(path: String): String =
      autoClose(Sourcer.sourceFromResource(path)) { source =>
        getTextFromSource(source)
      }

  def getTextFromFile(file: File): String =
      autoClose(Sourcer.sourceFromFile(file)) { source =>
        getTextFromSource(source)
      }

  def getTextFromFile(path: String): String =
      autoClose(Sourcer.sourceFromFile(path)) { source =>
        getTextFromSource(source)
      }

  def loadYamlFromResource(path: String): Collection[Any] = {
    val input = getTextFromResource(path)
    val yaml = new Yaml(new Constructor(classOf[Collection[Any]]))

    yaml.load(input).asInstanceOf[Collection[Any]]
  }

  def writeToJSONLD(annotatedDocuments: Seq[AnnotatedDocument], pw: PrintWriter, reader: EidosSystem): Unit = {
    // 4. Convert to JSON
    val corpus = new JLDCorpus(annotatedDocuments, reader)
    val mentionsJSONLD = corpus.serialize()
    // 5. Write to output file
    pw.println(stringify(mentionsJSONLD, pretty = true))
  }

  def copyResourceToFile(src: String, dest: File): Unit = {
    autoClose(FileUtils.getClass.getResourceAsStream(src)) { is: InputStream =>
      autoClose(new FileOutputStream(dest)) { os: FileOutputStream =>
        var buf = new Array[Byte](8192)
        var continue = true

        while (continue) {
          val len = is.read(buf)

          continue =
              if (len > 0) {
                os.write(buf, 0, len);
                true
              }
              else false
        }
      }
    }
  }

  def load[A](filename: String, classProvider: Any = this): A = {
    val classLoader = classProvider.getClass().getClassLoader()

    autoClose(new FileInputStream(filename)) { fileInputStream =>
      autoClose(new ClassLoaderObjectInputStream(classLoader, fileInputStream)) { objectInputStream =>
        objectInputStream.readObject().asInstanceOf[A]
      }
    }
  }
}
