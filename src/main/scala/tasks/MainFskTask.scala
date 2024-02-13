package tasks

import hackrf.{HackRFSweepDataCallback, HackRFSweepNativeBridge}
import javafx.concurrent.Task

class MainFskTask(centerFrequncyHz: Int, sampleRate: Int, fftSize: Int, lna: Int, vga: Int) extends Task[List[(Double, Float)]] with HackRFSweepDataCallback {
  //  start: 105000000 end: 112999938 with bin width: 61
  override def newSpectrumData(frequencyStart: Array[Double], signalPowerdBm: Array[Float]): Unit = {
    updateValue {
//      println(s"start: ${frequencyStart.head.toInt} end: ${frequencyStart.last.toInt} bin width: " +
//        s"${frequencyStart.drop(1).head.toInt - frequencyStart.head.toInt} size: ${signalPowerdBm.length} == ${frequencyStart.length}")
      frequencyStart.zip(signalPowerdBm).toList
    }

    if (isCancelled) {
      println("stoping the task")
      HackRFSweepNativeBridge.stop()
      //      oos.close()
    }
  }

  override def call(): List[(Double, Float)] = {
    HackRFSweepNativeBridge.start(this, centerFrequncyHz, sampleRate, fftSize, lna, vga)
    println("task completed!!")
    HackRFSweepNativeBridge.stop()
    Nil
  }
}

object MainFskTask {

}