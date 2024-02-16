package tasks

import hackrf.{HackRFSweepDataCallback, HackRFSweepNativeBridge}
import javafx.concurrent.Task
import mavlib.Batterworth2pLPF
import utils.Utils._

import java.util.concurrent.atomic.AtomicBoolean

class MainFskTask(startFrequncyHz: Int, sampleRate: Int, fftSize: Int, lna: Int, vga: Int, bw: Int, amountCount: Int, isOn: Boolean) extends Task[List[(Double, Double)]] with HackRFSweepDataCallback {
  val isOnAtomic = new AtomicBoolean(isOn)

  def updateOnOff(isOn: Boolean): Unit = {
    isOnAtomic.set(isOn)
  }

  val next: (Seq[(Double, Double)], Double, Int) => Double =
    (xs, base, i) => xs.dropWhile(_._1 < base + i).head._2
  val lengthFunc: (Array[Double], Int) => Array[Double] =
    (ar, i) => ar.takeWhile(_ <= startFrequncyHz + i)
  val step = 1000 // in Hz
  //  val buff = ArrayBuffer.fill(fftSize)(100.0)
  var count = 0
  val filter = new Batterworth2pLPF()
  filter.setCutoffFreqFactor(0.03)
  filter.reset(100)

  override def newSpectrumData(frequencyDomain: Array[Double], signalPowerdBm: Array[Double]): Unit = {
    val min = Math.abs(signalPowerdBm.min)
    //    val max = Math.abs(signalPowerdBm.max)
    val signalNorm = signalPowerdBm.map { el =>
      val n = min - Math.abs(el)
      filter.apply(n)
    }
    count += 1
    if (count % amountCount == 0) {
      count = 0
      if (isOnAtomic.get()) {
        updateValue {
          //      println(s"start: ${frequencyStart.head.toInt} end: ${frequencyStart.last.toInt} bin width: " +
          //        s"${frequencyStart.drop(1).head.toInt - frequencyStart.head.toInt} size: ${signalPowerdBm.length} == ${frequencyStart.length}")
          frequencyDomain.zip(signalNorm).toList
        }
      }

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
            //          val calcCenterRaw = findMeanF(l.map(_._2))
            val calcCenterRaw = calcCoefAmplBand(l.map(_._2))
            val calcCenter = BigDecimal(calcCenterRaw).setScale(1, BigDecimal.RoundingMode.HALF_DOWN).toDouble
            //            val calcCenter = calcCenterRaw
            val center = if (calcCenter == 0) 0.01 else calcCenter
            val headFreq = l.head._1
            val base = next.curried(l).apply(headFreq)
            //        val s = base(0)
            //        val f = base(4000)
            //        val five = base(8000)
            //        val ten = l.last
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
//            res
          }
      }
    }

    if (isCancelled) {
      println("stoping the task")
      HackRFSweepNativeBridge.stop()
    }
  }

  override def call(): List[(Double, Double)] = {
    HackRFSweepNativeBridge.start(this, startFrequncyHz, sampleRate, fftSize, lna, vga, bw)
    println("task completed!!")
    HackRFSweepNativeBridge.stop()
    Nil
  }
}

object MainFskTask {

}