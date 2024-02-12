package tasks

import hackrf.{HackRFSweepDataCallback, HackRFSweepNativeBridge}
import javafx.concurrent.Task

class MainFskTask(centerFrequncyHz: Int, fftBinWidth: Int, lna: Int, vga: Int) extends Task[List[(Double, Float)]] with HackRFSweepDataCallback {

  override def newSpectrumData(frequencyStart: Array[Double], signalPowerdBm: Array[Float]): Unit = {
    updateValue {
      frequencyStart.zip(signalPowerdBm).toList
    }

    if (isCancelled) {
      println("stoping the task")
      HackRFSweepNativeBridge.stop()
      //      oos.close()
    }
  }

  override def call(): List[(Double, Float)] = {
    HackRFSweepNativeBridge.start(this, centerFrequncyHz, 10000000, 0, lna, vga)
    println("task completed!!")

    Nil
  }
}

object MainFskTask {

}