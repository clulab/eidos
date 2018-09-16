package org.clulab.wm.eidos.groundings

import java.io.{FileInputStream, ObjectInputStream}

import org.clulab.utils.ClassLoaderObjectInputStream
import org.clulab.wm.eidos.utils.Namer
import org.slf4j.LoggerFactory

trait DomainOntology {

  def size: Integer

  def getNamer(n: Integer): Namer

  def getValues(n: Integer): Array[String]

  def save(filename: String): Unit
}

object DomainOntology {
  val ESCAPE = "\\"
  val ESCAPED_ESCAPE = ESCAPE + ESCAPE
  val SEPARATOR = "/"
  val ESCAPED_SEPARATOR = ESCAPE + SEPARATOR

  val logger = LoggerFactory.getLogger(this.getClass())

  // fixme: the following lines are almost directly from Serializer.  We were having problems with the ClassLoader not
  // being able to find DomainOntology from within the webapp submodule, so we put it here.  This is not ideal
  // and should be fixed, probably in Processors.
  def updatedLoad[A](filename: String, classProvider: Any = this): A = {
    logger.info(s"Loading serialized Ontology from $filename")

    val classLoader = classProvider.getClass().getClassLoader()
    val fileInputStream = new FileInputStream(filename)
    var objectInputStream: ObjectInputStream = null

    try {
      objectInputStream = new ClassLoaderObjectInputStream(classLoader, fileInputStream)

      objectInputStream.readObject().asInstanceOf[A]
    }
    finally {
      if (objectInputStream != null)
        objectInputStream.close()
      else
        fileInputStream.close()
      logger.info("Serialized Ontology successfully loaded.")
    }
  }
}