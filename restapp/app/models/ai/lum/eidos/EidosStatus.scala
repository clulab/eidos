package ai.lum.eidos

class EidosStatus {
  protected var count: Int = 0

  def inc: Unit = this.synchronized {
    count += 1
  }

  def get: Int = this.synchronized {
    count
  }
}
