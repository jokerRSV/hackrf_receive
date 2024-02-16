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

//  val detectorFSKTask = new DetectorFSKTask(startFrequncyHz)
  var count = 0
  val filter = new Batterworth2pLPF()
  filter.setCutoffFreqFactor(0.03)
  filter.reset(100)

  override def newSpectrumData(frequencyDomain: Array[Double], signalPowerdBm: Array[Double]): Unit = {
    //    val min = Math.abs(signalPowerdBm.min)
    val signalNorm = signalPowerdBm.map { el =>
      filter.apply(el)
    }
    count += 1
    if (count % amountCount == 0) {
      count = 0
      if (isOnAtomic.get()) {
        updateValue {
          frequencyDomain.zip(signalNorm).toList
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