package org.clulab.wm.eidos.groundings

import java.io.{FileInputStream, FileOutputStream, ObjectOutputStream}
import java.util.IdentityHashMap

import org.clulab.utils.ClassLoaderObjectInputStream
import org.clulab.wm.eidos.utils.Namer

import scala.collection.JavaConverters._
import scala.collection.mutable.{HashMap => MutableHashMap}
import scala.collection.mutable.ArrayBuffer

@SerialVersionUID(1000L)
class CompactDomainOntology(protected val strings: Array[String], protected val leafStringIndexes: Array[Int],
    protected val leafIndexes: Array[Int], protected val branchIndexes: Array[Int]) extends DomainOntology with Serializable {

  class InnerNamer(protected val n: Int) extends Namer {

    protected def parentName(n: Int, stringBuilder: StringBuilder): Unit = {
      if (n >= 0) {
        val index = n * CompactDomainOntology.branchIndexWidth

        parentName(branchIndexes(index + CompactDomainOntology.branchParentOffset), stringBuilder)
        stringBuilder.append(strings(branchIndexes(index + CompactDomainOntology.branchNameOffset)))
        stringBuilder.append(DomainOntology.SEPARATOR)
      }
    }

    def name: String = {
      val stringBuilder = new StringBuilder()
      val index = n * CompactDomainOntology.leafIndexWidth
      val nameOffset = leafIndexes(index + CompactDomainOntology.leafStringStartOffset) + CompactDomainOntology.leafNameOffset

      parentName(leafIndexes(index + CompactDomainOntology.leafParentOffset), stringBuilder)
      stringBuilder.append(strings(leafStringIndexes(nameOffset)))
      stringBuilder.result()
    }
  }

  def size: Integer = leafIndexes.size / CompactDomainOntology.leafIndexWidth - 1

  def getNamer(n: Integer): Namer = new InnerNamer(n)

  def getValues(n: Integer): Array[String] = {
    val startIndex = n * CompactDomainOntology.leafIndexWidth + CompactDomainOntology.leafStringStartOffset
    val stopIndex = startIndex + CompactDomainOntology.leafIndexWidth
    val start =  leafIndexes(startIndex) + CompactDomainOntology.leafValueOffset
    val stop = leafIndexes(stopIndex)

    start.until(stop).toArray.map(n => strings(leafStringIndexes(n)))
  }

  def save(filename: String) = {
    val objectOutputStream = new ObjectOutputStream(new FileOutputStream(filename))

    objectOutputStream.writeObject(strings.mkString("\n"))
    objectOutputStream.writeObject(leafStringIndexes)
    objectOutputStream.writeObject(leafIndexes)
    objectOutputStream.writeObject(branchIndexes)
    objectOutputStream.close()
  }
}

object CompactDomainOntology {
  // First string is always the name of the node
  val leafNameOffset = 0
  val leafValueOffset = 1

  // leafIndexes are really (leafParentIndex, leafStringStartIndex)
  val leafIndexWidth = 2
  val leafParentOffset = 0
  val leafStringStartOffset = 1
  // There is an extra of the above with the last leafStringStartIndex

  // branchIndexes are really (branchNameIndex, branchParentIndex)
  val branchIndexWidth = 2
  val branchParentOffset = 0
  val branchNameOffset = 1

  def load(filename: String): CompactDomainOntology = {
    val objectInputStream = new ClassLoaderObjectInputStream(this.getClass().getClassLoader(), new FileInputStream(filename))
    val arrayBuffer = new ArrayBuffer[String]()

    {
      // This is so that text can be abandoned at the end of the block, before the array is read.
      val text = objectInputStream.readObject().asInstanceOf[String]
      val stringBuilder = new StringBuilder
      var count = 0

      for (i <- 0 until text.size) {
        val c = text(i)

        if (c == '\n') {
          arrayBuffer += stringBuilder.result()
          count += 1
          stringBuilder.clear()
        }
        else
          stringBuilder.append(c)
      }
      arrayBuffer += stringBuilder.result()
    }
    val strings = arrayBuffer.toArray
    val leafStringIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]
    val leafIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]
    val branchIndexes = objectInputStream.readObject().asInstanceOf[Array[Int]]

    objectInputStream.close
    new CompactDomainOntology(strings, leafStringIndexes, leafIndexes, branchIndexes)
  }

  class CompactDomainOntologyBuilder(treeDomainOntology: TreeDomainOntology) {

    protected def append(strings: MutableHashMap[String, Int], string: String): Unit =
       if (!strings.contains(string))
          strings.put(string, strings.size)

    protected def newParentMap(): IdentityHashMap[OntologyBranchNode, (Int, Int)] = {
      // This is myIndex, parentIndex
      val parentMap: IdentityHashMap[OntologyBranchNode, (Int, Int)] = new IdentityHashMap()

      def append(parents: Seq[OntologyBranchNode]): Int =
          if (parents.nonEmpty)
            if (parentMap.containsKey(parents.head))
              parentMap.get(parents.head)._1
            else {
              val parentIndex = append(parents.tail) // Put root on top.
              val myIndex = parentMap.size
              parentMap.put(parents.head, (myIndex, parentIndex))
              myIndex
            }
          else
            -1

      0.until(treeDomainOntology.size).foreach { i =>
        append(treeDomainOntology.getParents(i))
      }
      parentMap
    }

    protected def newStringMap(parentMap: IdentityHashMap[OntologyBranchNode, (Int, Int)]): MutableHashMap[String, Int] = {
      val stringMap: MutableHashMap[String, Int] = new MutableHashMap()

      parentMap.keySet().forEach { ontologyBranchNode =>
        append(stringMap, ontologyBranchNode.escaped)
      }

      0.until(treeDomainOntology.size).foreach { i =>
        append(stringMap, treeDomainOntology.getNode(i).escaped)
        treeDomainOntology.getValues(i).foreach(append(stringMap, _))
      }
      stringMap
    }

    protected def newLeafIndexes(parentMap: IdentityHashMap[OntologyBranchNode, (Int, Int)], stringMap: MutableHashMap[String, Int]): (Array[Int], Array[Int]) = {
      val stringIndexBuffer = new ArrayBuffer[Int]()
      val indexBuffer = new ArrayBuffer[Int]()

      0.until(treeDomainOntology.size).foreach { i =>
        val node = treeDomainOntology.getNode(i)

        indexBuffer += parentMap.get(node.parent)._1 // leafParentOffset
        indexBuffer += stringIndexBuffer.size // leafStringStartOffset

        stringIndexBuffer += stringMap.get(node.escaped).get // leafName
        treeDomainOntology.getValues(i).foreach { value =>
          stringIndexBuffer += stringMap.get(value).get // leafValues
        }
      }
      indexBuffer += -1 // extra leafParentOffset
      indexBuffer += stringIndexBuffer.size // extra leafStringStartOffset
      (stringIndexBuffer.toArray, indexBuffer.toArray)
    }

    protected def newBranchIndexes(parentMap: IdentityHashMap[OntologyBranchNode, (Int, Int)], stringMap: MutableHashMap[String, Int]): Array[Int] = {
      val indexBuffer = new ArrayBuffer[Int]()
      val keysAndValues: Array[(OntologyBranchNode, (Int, Int))] = parentMap.asScala.toArray.sortBy(_._2._1)

      keysAndValues.foreach { case (branchNode, (myIndex, parentIndex)) =>
        indexBuffer += parentIndex // branchParentOffset
        indexBuffer += stringMap.get(branchNode.escaped).get // branchNameOffset
      }
      indexBuffer.toArray
    }

    def build(): DomainOntology = {
      val parentMap: IdentityHashMap[OntologyBranchNode, (Int, Int)] = newParentMap()
      val stringMap: MutableHashMap[String, Int] = newStringMap(parentMap)
      val (leafStringIndexes, leafIndexes) = newLeafIndexes(parentMap, stringMap)
      val branchIndexes = newBranchIndexes(parentMap, stringMap)
      val strings: Array[String] = stringMap.toArray.sortBy(_._2).map(_._1)

      new CompactDomainOntology(strings, leafStringIndexes, leafIndexes, branchIndexes)
    }
  }
}
