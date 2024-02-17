package tasks

import hackrf.{HackRFSweepDataCallback, HackRFSweepNativeBridge}
import javafx.concurrent.Task
import mavlib.Batterworth2pLPF

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.collection.mutable.ArrayBuffer

class MainFskTask(startFrequncyHz: Int, sampleRate: Int, fftSize: Int, lna: Int, vga: Int, bw: Int, amountCount: Int,
                  isOn: Boolean, factor: Double, detectorFSKTask: Option[DetectorFSKTask]) extends Task[(Array[(Double, Double)], Boolean, Double)] with HackRFSweepDataCallback {
  val isOnAtomic = new AtomicBoolean(isOn)
  val counterLimitAtomic = new AtomicInteger(amountCount)
  val filterFactorAtomic = new AtomicReference(0.03)

  def updateOnOff(isOn: Boolean): Unit = {
    isOnAtomic.set(isOn)
  }

  def updateFilterFactor(factor: Double): Unit = {
    filterFactorAtomic.set(factor)
    filter.setCutoffFreqFactor(factor)
    filter.reset(-100)
  }

  def updateCounterLimit(c: Int): Unit = {
    counterLimitAtomic.set(c)
  }

  var count = 0
  val filter = new Batterworth2pLPF()
  updateFilterFactor(factor)
  var buff = ArrayBuffer.fill(fftSize)(100.0)

  override def newSpectrumData(frequencyDomain: Array[Double], signalPowerdBm: Array[Double], sweepDone: Boolean, fftBinWidth: Double): Unit = {
    println(s"${frequencyDomain.head}___${signalPowerdBm.head}")
    buff = buff.zip(signalPowerdBm).map { tuple =>
      //remove minus value
      val mean = (tuple._1 - tuple._2) / 2
      val v = filter.apply(mean)
      v
    }
    val zipped = frequencyDomain.zip(buff)
    detectorFSKTask.foreach(_.updateFreqDomain(zipped))

    count += 1
    if (count % counterLimitAtomic.get() == 0) {
      count = 0
      if (isOnAtomic.get()) {
        updateValue {
          (zipped, sweepDone, fftBinWidth)
        }
      }
    }

    if (isCancelled) {
      println("stoping the task")
      HackRFSweepNativeBridge.stop()
    }
  }

  override def call(): (Array[(Double, Double)], Boolean, Double) = {
    HackRFSweepNativeBridge.start(this, startFrequncyHz, sampleRate, fftSize, lna, vga, bw)
    println("task completed!!")
    HackRFSweepNativeBridge.stop()
    (Array.empty, false, 0d)
  }
}

object MainFskTask {

}