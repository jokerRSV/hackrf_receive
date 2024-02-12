package model

import scala.collection.mutable.ArrayBuffer

case class FftBin(size: Int = 8) {
  val scale = ArrayBuffer.fill(size)(Array.empty[Double])
  val power = ArrayBuffer.fill(size)(Array.empty[Float])
  val buff = ArrayBuffer.empty[ArrayBuffer[Float]] //.fill(0)(Array.empty[Float])

  def updateScale(scaleFreqs: Array[Double], powers: Array[Float], band: Int): Unit = {
    try {
      scale.update(band - 1, scaleFreqs.init)
      power.update(band - 1, powers.init)
    } catch {
      case e: Exception => throw e
    }
  }

  def add(): Unit = {
    buff.addOne(power.flatten)
  }

  def getFlatten: (ArrayBuffer[Double], ArrayBuffer[Float]) = {
    (scale.flatten, power.flatten)
  }

  def getFlattenScaleMean: (ArrayBuffer[Double], ArrayBuffer[Float]) = {
    val p = buff.apply(0).zip(buff.apply(1)).zip(buff.apply(2)).zip(buff.apply(3)).zip(buff.apply(4)).map {
      case ((((fi, fi1), s), t), f) =>
        (fi + fi1 + s + t + f) / 4
    }
    buff.clear()
    (scale.flatten, p)
  }

}
