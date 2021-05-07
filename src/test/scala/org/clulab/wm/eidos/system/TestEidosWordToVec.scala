package org.clulab.wm.eidos.system

import org.clulab.wm.eidoscommon.utils.Test

class TestEidosWordToVec extends Test {

  behavior of "CompactWordEmbeddingMap types"

  it should "work for Array" in {
    val length = 3
    val implArray = new Array[Float](length)
    val array = implArray.asInstanceOf[Array[Float]]

    println(array(length - 1))
  }

  it should "work for Seq" in {
    val length = 3
    val implArray = new Array[Float](length)
    val implSeq: IndexedSeq[Float] = implArray
    val seq = implSeq.asInstanceOf[Seq[Float]]

    println(seq(length - 1))
  }

  behavior of "ExplicitWordEmbeddingMap types"

  it should "work for Array" in {
    val length = 3
    val implArray = new Array[Float](length)
    val array = implArray.asInstanceOf[Array[Float]]

    println(array(length - 1))
  }

  it should "work for Seq" in {
    val length = 3
    val implArray = new Array[Float](length)
    val implSeq: IndexedSeq[Float] = implArray
    val seq = implSeq.asInstanceOf[Seq[Float]]

    println(seq(length - 1))
  }
}
