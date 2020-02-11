package ai.lum.eidos.sparql.utils

class ShortTermMemory[T](protected var valueOpt: Option[T]) {

  def isSame(newValue: T): Boolean = {
    val same = valueOpt.map(_ == newValue).getOrElse(false)

    valueOpt = Some(newValue)
    same
  }

  def isDifferent(newValue: T): Boolean = {
    val different = valueOpt.map(_ != newValue).getOrElse(true)

    valueOpt = Some(newValue)
    different
  }
}

object ShortTermMemory {

  def apply[T]: ShortTermMemory[T] = new ShortTermMemory[T](None)

  def apply[T](value: T): ShortTermMemory[T] = new ShortTermMemory[T](Some(value))
}