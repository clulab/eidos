package ai.lum.eidos

class EidosStatus {
  protected var busy: Boolean = false

  def get: Boolean = this.synchronized {
    busy
  }

  def start: Boolean = this.synchronized {
    val oldBusy = busy

    busy = true
    oldBusy
  }

  def stop: Boolean = this.synchronized {
    val oldBusy = busy

    busy = false
    oldBusy
  }
}
