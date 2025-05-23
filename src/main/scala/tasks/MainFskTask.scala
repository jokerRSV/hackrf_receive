package tasks

import hackrf.{HackRFSweepDataCallback, HackRFSweepNativeBridge}
import javafx.concurrent.Task
import mavlib.Batterworth2pLPF

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

class MainFskTask(startFrequncyHz: Int, sampleRate: Int, fftBinWidth: Int, lna: Int, vga: Int, bw: Int, amountCount: Int,
                  isOn: Boolean, factor: Double, detectorFSKTask: Option[DetectorFSKTask], ampEnable: Boolean, noLogs: Boolean)
  extends Task[(Array[(Double, Double)], Double, Int)] with HackRFSweepDataCallback {
  if (!noLogs)
    println("init MainFskTask")
  val isOnAtomic = new AtomicBoolean(isOn)
  val counterLimitAtomic = new AtomicInteger(amountCount)
  val filterFactorAtomic = new AtomicReference(factor)

  def updateOnOff(isOn: Boolean): Unit = {
    isOnAtomic.set(isOn)
  }

  def updateFilterFactor(factorUpdated: Double): Unit = {
    filterFactorAtomic.set(factorUpdated)
    filter.setCutoffFreqFactor(factorUpdated)
    filter.reset(factorUpdated)
  }

  def updateCounterLimit(c: Int): Unit = {
    counterLimitAtomic.set(c)
  }

  var count = 0
  val filter = new Batterworth2pLPF()
  updateFilterFactor(factor)

  override def newSpectrumData(frequencyDomain: Array[Double], signalPowerdBm: Array[Double], fftSize: Int, bandWidth: Int): Unit = {
    //    println(s"${frequencyDomain.head}___${frequencyDomain.last}")
    filter.reset(0)
    val buff =
      frequencyDomain
        .zip(signalPowerdBm)
        .takeWhile(_._1 <= startFrequncyHz + bandWidth)
        .drop(5)
        .map(tuple => (tuple._1, filter.apply(tuple._2)))
    val zipped = buff
    detectorFSKTask.foreach(_.updateFreqDomain((zipped, fftBinWidth, bandWidth)))

    count += 1
    if (count % counterLimitAtomic.get() == 0) {
      count = 0
      if (isOnAtomic.get()) {
        updateValue {
          (zipped, fftSize, bandWidth)
        }
      }
    }

    if (isCancelled) {
      if (!noLogs)
        println("stoping main fsk task")
      HackRFSweepNativeBridge.stop()
    }
  }

  override def call(): (Array[(Double, Double)], Double, Int) = {
    if (!noLogs)
      println("start thread MainFskTask")
    HackRFSweepNativeBridge.start(this, startFrequncyHz, sampleRate, fftBinWidth, lna, vga, bw, ampEnable, noLogs)
    (Array.empty, 0d, 0)
  }
}

object MainFskTask {

}