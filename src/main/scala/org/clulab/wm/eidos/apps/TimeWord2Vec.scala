package org.clulab.wm.eidos.apps

import java.io.{FileInputStream, ObjectInputStream}

import org.clulab.embeddings.word2vec.{Word2Vec => RemoteWord2Vec}
import org.clulab.utils.{ClassLoaderObjectInputStream, Serializer}
import org.clulab.wm.eidos.groundings.{CompactWord2Vec => LocalWord2Vec}
import org.clulab.wm.eidos.utils.{Sourcer, Timer}

object TimeWord2Vec extends App {

  def updatedLoad[A](filename: String, classProvider: Any = this): A = {
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
    }
  }

//  val filename = "/org/clulab/wm/eidos/w2v/vectors.txt"
//  val serializedFilename = "/D:/vectors.txt.serialized"

  val filename = "/org/clulab/wm/eidos/w2v/glove.840B.300d.vectors.txt"
  val serializedFilename = "/D:/glove.840B.300d.vectors.txt.serialized"

//  val localWord2Vec1 = Timer.time("How long does it take to load local?") {
//      LocalWord2Vec(filename, resource = true, cached = false)
//  }

//  Timer.time("How long to serialize out?") {
//    localWord2Vec1.save(serializedFilename)
//  }

  val localWord2Vec2 = Timer.time("How long to serialize in?") {
    LocalWord2Vec(serializedFilename, resource = true, cached = true)
  }

//  assert(localWord2Vec1.keys.size == localWord2Vec2.keys.size)
//  var differCount = 0
//  localWord2Vec1.keys.foreach { key =>
//    val array1 = localWord2Vec1.get(key).get
//    val array2 = localWord2Vec2.get(key).get
//    val differs = 0.until(array1.size).exists(i => array1(i) != array2(i))
//
//    if (differs) {
//      differCount += 1
//      println(key)
//      println(array1.map(_.toString).mkString(" "))
//      println(array2.map(_.toString).mkString(" "))
//    }
//  }
//  println("Differences: " + differCount)

  val remoteWord2Vec = Timer.time("How long does it take to load remote?") {
    val source = Sourcer.sourceFromResource(filename)

    try {
      new RemoteWord2Vec(source, None)
    }
    finally {
      source.close()
    }
  }

  remoteWord2Vec.matrix.keys.foreach { key =>
    val localArray = localWord2Vec2.get(key).get
    val remoteArray = remoteWord2Vec.matrix(key)
//    val differs = 0.until(localArray.size).exists(i => localArray(i) != remoteArray(i))
    // Account for float/double difference.
    val differs = 0.until(localArray.size).exists(i => math.abs(localArray(i) - remoteArray(i)) > 0.00001)

    if (differs) {
      println(key)
      println(localArray.map(_.toString).mkString(" "))
      println(remoteArray.map(_.toString).mkString(" "))
    }
  }
}
