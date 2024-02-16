package tasks

import hackrf.{HackRFSweepDataCallback, HackRFSweepNativeBridge}
import javafx.concurrent.Task
import mavlib.Batterworth2pLPF

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.collection.mutable.ArrayBuffer

class MainFskTask(startFrequncyHz: Int, sampleRate: Int, fftSize: Int, lna: Int, vga: Int, bw: Int, amountCount: Int, isOn: Boolean) extends Task[Array[(Double, Double)]] with HackRFSweepDataCallback {
  val isOnAtomic = new AtomicBoolean(isOn)
  val counterLimitAtomic = new AtomicInteger(amountCount)

  def updateOnOff(isOn: Boolean): Unit = {
    isOnAtomic.set(isOn)
  }

  def updateCounterLimit(c: Int): Unit = {
    counterLimitAtomic.set(c)
  }

  //  val detectorFSKTask = new DetectorFSKTask(startFrequncyHz)
  var count = 0
  val filter = new Batterworth2pLPF()
  filter.setCutoffFreqFactor(0.01)
  filter.reset(-100)
  var buff = ArrayBuffer.fill(fftSize)(-100.0)

  override def newSpectrumData(frequencyDomain: Array[Double], signalPowerdBm: Array[Double]): Unit = {
    //    val min = Math.abs(signalPowerdBm.min)
    buff = buff.zip(signalPowerdBm).map { tuple =>
      val mean = (tuple._1 + tuple._2) / 2
      val v = filter.apply(mean)
      v
    }
    count += 1
    if (count % counterLimitAtomic.get() == 0) {
      count = 0
      if (isOnAtomic.get()) {
        updateValue {
          frequencyDomain.zip(buff)
        }
      }
    }

    if (isCancelled) {
      println("stoping the task")
      HackRFSweepNativeBridge.stop()
    }
  }

  override def call(): Array[(Double, Double)] = {
    HackRFSweepNativeBridge.start(this, startFrequncyHz, sampleRate, fftSize, lna, vga, bw)
    println("task completed!!")
    HackRFSweepNativeBridge.stop()
    Array.empty
  }
}

object MainFskTask {

}