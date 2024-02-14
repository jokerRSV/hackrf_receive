package tasks

import hackrf.{HackRFSweepDataCallback, HackRFSweepNativeBridge}
import javafx.concurrent.Task
import utils.Utils._

import scala.collection.mutable.ArrayBuffer

class MainFskTask(startFrequncyHz: Int, sampleRate: Int, fftSize: Int, lna: Int, vga: Int, bw: Int, amountCount: Int) extends Task[List[(Double, Float)]] with HackRFSweepDataCallback {
  val next: (Seq[(Double, Float)], Double, Int) => Float =
    (xs, base, i) => xs.dropWhile(_._1 < base + i).head._2
  val lengthFunc: (Array[Double], Int) => Array[Double] =
    (ar, i) => ar.takeWhile(_ <= startFrequncyHz + i)
  val step = 1000 // in Hz
  val buff = ArrayBuffer.fill(fftSize)(0f)
  var count = 0

  override def newSpectrumData(frequencyDomain: Array[Double], signalPowerdBm: Array[Float]): Unit = {
    signalPowerdBm.zipWithIndex.foreach(t => buff.update(t._2, (buff.apply(t._2) + t._1) / 2))
    count += 1
    if (count % amountCount == 0) {
      count = 0
      updateValue {
        //      println(s"start: ${frequencyStart.head.toInt} end: ${frequencyStart.last.toInt} bin width: " +
        //        s"${frequencyStart.drop(1).head.toInt - frequencyStart.head.toInt} size: ${signalPowerdBm.length} == ${frequencyStart.length}")
        frequencyDomain.zip(buff).toList
      }

      val fskLengthFull = lengthFunc(frequencyDomain, 12000).length
      val fskLength1kHz = lengthFunc(frequencyDomain, step)

      frequencyDomain
        .zip(buff)
        .dropWhile(_._1 % step != 0)
        .sliding(fskLengthFull, fskLength1kHz.length - 1)
        .toList
        .filter(_.length == fskLengthFull)
        .exists { l =>
          val calcCenterRaw = findMeanF(l.map(_._2))
          val calcCenter = BigDecimal(calcCenterRaw).setScale(1, BigDecimal.RoundingMode.HALF_DOWN).toDouble
          val center = if (calcCenter == 0) 0.01 else calcCenter
          val headFreq = l.head._1
          val base = next.curried(l).apply(headFreq)
          //        val s = base(0)
          //        val f = base(4000)
          //        val five = base(8000)
          //        val ten = l.last
          val res =
          base(0) >= center &&
            base(4000) >= center &&
            base(8000) >= center &&
            l.last._2 >= center &&
            base(2000) < center &&
            base(6000) < center &&
            base(10000) < center
          if (res) {
            println(s"find start freq: ${l.head._1.toInt} level: ${calcCenter}\n")
          }
          res
        }
    }

    if (isCancelled) {
      println("stoping the task")
      HackRFSweepNativeBridge.stop()
    }
  }

  override def call(): List[(Double, Float)] = {
    HackRFSweepNativeBridge.start(this, startFrequncyHz, sampleRate, fftSize, lna, vga, bw)
    println("task completed!!")
    HackRFSweepNativeBridge.stop()
    Nil
  }
}

object MainFskTask {

}