package org.clulab.wm.eidos.groundings

import java.util.IdentityHashMap

import org.clulab.utils.Serializer
import org.clulab.wm.eidos.utils.Namer

import scala.collection.JavaConverters._
import scala.collection.mutable.{HashMap => MutableHashMap}
import scala.collection.mutable.ArrayBuffer

@SerialVersionUID(1000L)
class CompactDomainOntology(protected val strings: Array[String], protected val leafStringIndexes: Array[Int],
    protected val leafIndexes: Array[Int], protected val branchIndexes: Array[Int]) extends DomainOntology with Serializable {

  // Put these into an object
  // leafIndexes are really (leafParentIndex, leafStringStartIndex)
  // First string is always the name of the node
  // There is an extra of the above with the last leafStringStartIndex
  // branchIndexes are really (branchNameIndex, branchParentIndex)

  class InnerNamer(protected val n: Int) extends Namer {

    protected def parentName(n: Int, stringBuilder: StringBuilder): Unit = {
      if (n > 0) {
        parentName(branchIndexes(n * 2 + 0), stringBuilder)
        stringBuilder.append(strings(n * 2 + 0))
        stringBuilder.append(DomainOntology.SEPARATOR)
      }
    }

    def name: String = {
      val stringBuilder = new StringBuilder()

      parentName(leafIndexes(n + 4 + 1), stringBuilder)
      stringBuilder.append(strings(n * 4 + 0))
      stringBuilder.result()
    }
  }

  def size: Integer = (leafIndexes.size - 1) / 4

  def getNamer(n: Integer): Namer = new InnerNamer(n)

  def getValues(n: Integer): Array[String] = leafIndexes(n * 4 + 2).until(leafIndexes(n * 4 + 3)).toArray.map(strings(leafStringIndexes(_)))

  def save(filename: String) = {
    Serializer.save(this, filename) // Want to combine some arrays of strings into one single string
  }
}

object CompactDomainOntology {
  def load(path: String): CompactDomainOntology = DomainOntology.updatedLoad[CompactDomainOntology](path)

  class CompactDomainOntologyBuilder(treeDomainOntology: TreeDomainOntology) {
    // TODO, the node names are already escaped, while other strings aren't

    protected def append(strings: MutableHashMap[String, Int], string: String): Unit =
       if (!string.contains(string))
          strings.put(string, strings.size)

    protected def newParentMap(): IdentityHashMap[OntologyBranchNode, Int] = {
      val parentMap: IdentityHashMap[OntologyBranchNode, Int] = new IdentityHashMap()

      def append(parents: Seq[OntologyBranchNode]): Unit =
          if (parents.nonEmpty && !parentMap.containsKey(parents.head)) {
            parentMap.put(parents.head, parentMap.size)
            append(parents.tail)
          }

      0.until(treeDomainOntology.size).foreach { i =>
        append(treeDomainOntology.getParents(i))
      }
      parentMap
    }

    protected def newStringMap(parentMap: IdentityHashMap[OntologyBranchNode, Int]): MutableHashMap[String, Int] = {
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

    protected def newLeafIndexes(parentMap: IdentityHashMap[OntologyBranchNode, Int], stringMap: MutableHashMap[String, Int]): (Array[Int], Array[Int]) = {
      val stringIndexBuffer = new ArrayBuffer[Int]()
      val indexBuffer = new ArrayBuffer[Int]()

      0.until(treeDomainOntology.size).foreach { i =>
        val node = treeDomainOntology.getNode(i)

        indexBuffer += parentMap.get(node.parent)
        indexBuffer += stringIndexBuffer.size

        stringIndexBuffer += stringMap.get(node.escaped).get
        treeDomainOntology.getValues(i).foreach { value =>
          stringIndexBuffer += stringMap.get(value).get
        }
      }

      indexBuffer += -1
      indexBuffer += stringIndexBuffer.size

      (stringIndexBuffer.toArray, indexBuffer.toArray)
    }

    protected def newBranchIndexes(parentMap: IdentityHashMap[OntologyBranchNode, Int], stringMap: MutableHashMap[String, Int]): Array[Int] = {
      val indexBuffer = new ArrayBuffer[Int]()
      val keysAndValues: Array[(OntologyBranchNode, Int)] = parentMap.asScala.toArray.sortBy(_._2)

      keysAndValues.foreach { case (branchNode, index) =>
          indexBuffer += stringMap.get(branchNode.escaped).get
          indexBuffer += index
      }
      indexBuffer.toArray
    }

    def build(): DomainOntology = {
      val parentMap: IdentityHashMap[OntologyBranchNode, Int] = newParentMap()
      val stringMap: MutableHashMap[String, Int] = newStringMap(parentMap)
      val (leafIndexes, leafStringIndexes) = newLeafIndexes(parentMap, stringMap)
      val branchIndexes = newBranchIndexes(parentMap, stringMap)
      val strings: Array[String] = stringMap.toArray.sortBy(_._2).map(_._1)

      new CompactDomainOntology(strings, leafStringIndexes, leafIndexes, branchIndexes)
    }
  }
}
