package tasks

import javafx.concurrent.Task
import utils.Utils.calcCoefAmplBand

import java.util.concurrent.atomic.AtomicReference

class DetectorFSKTask(startFrequncyHz: Int) extends Task[Unit] {

  val next: (Seq[(Double, Double)], Double, Int) => Double =
    (xs, base, i) => xs.dropWhile(_._1 < base + i).head._2
  val lengthFunc: (Array[Double], Int) => Array[Double] =
    (ar, i) => ar.takeWhile(_ <= startFrequncyHz + i)
  val step = 1000 // in Hz

  val atomicFreqDomain = new AtomicReference[Tuple2[Array[Double], Array[Double]]]((Array.empty, Array.empty))

  def updateFreqDomain(frequencyDomain: Array[Double], signalPowerdBm: Array[Double]): Unit = {
    atomicFreqDomain.set((frequencyDomain, signalPowerdBm))
  }

  override def call(): Unit = {
    def loop(): Unit = {
      val frequencyDomain = atomicFreqDomain.get()._1
      val signalNorm = atomicFreqDomain.get()._2
      val fskLengthFull = lengthFunc(frequencyDomain, 12000).length
      val fskLength1kHz = lengthFunc(frequencyDomain, step)
      if (fskLength1kHz.length > 1) {
        frequencyDomain
          .zip(signalNorm)
          .dropWhile(_._1 % step != 0)
          .sliding(fskLengthFull, fskLength1kHz.length - 1)
          .toList
          .filter(_.length == fskLengthFull)
          .foreach { l =>
            val calcCenterRaw = calcCoefAmplBand(l.map(_._2))
            val calcCenter = BigDecimal(calcCenterRaw).setScale(1, BigDecimal.RoundingMode.HALF_DOWN).toDouble
            val center = if (calcCenter == 0) 0.01 else calcCenter
            val headFreq = l.head._1
            val base = next.curried(l).apply(headFreq)
            val b0 = base(0)
            val b4 = base(4000)
            val b8 = base(8000)
            val b12 = l.last._2
            val b2 = base(2000)
            val b6 = base(6000)
            val b10 = base(10000)
            val res =
              b0 >= center &&
                b4 >= center &&
                b8 >= center &&
                b12 >= center &&
                b2 < center &&
                b6 < center &&
                b10 < center
            if (res) {
              println(s"find start freq: ${l.head} end: ${l.last} center lvl: ${calcCenter}_____b0: ${b0}__b4: ${b4}__b4: ${b4}__b12: ${b12}______b2: ${b2}__b6: ${b6}__b10: ${b10}")
            }
          }
      }
    }

    if (!isCancelled) {
      loop()
    } else {
      println("completing detector task")
    }
  }
}
