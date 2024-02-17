package tasks

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.collection.parallel.CollectionConverters._

class DetectorFSKTask(startFrequncyHz: Int) extends Runnable {
  println("init detector")
  val next: (Seq[(Double, Double)], Double, Int) => Double =
    (xs, base, i) => xs.dropWhile(_._1 < base + i).head._2
  val lengthFunc: (Array[(Double, Double)], Int) => Array[(Double, Double)] =
    (ar, i) => ar.takeWhile(_._1 <= startFrequncyHz + i)
  val step = 1000 // in Hz
  val levelOne = -70

  val atomicFreqDomain = new AtomicReference[Array[(Double, Double)]](Array.empty)
  val isCancelled = new AtomicBoolean(false)

  def updateFreqDomain(zippedFreqDomain: Array[(Double, Double)]): Unit = {
    val v = atomicFreqDomain.set(zippedFreqDomain)
    v
  }

  def cancel(): Unit = {
    println("stopping detector task")
    isCancelled.set(true)
  }

  override def run(): Unit = {
    println("start detector task")

    def loop(): Unit = {
      val frequencyDomain = atomicFreqDomain.get()
      val fskLengthFull = lengthFunc(frequencyDomain, 12000).length
      val fskLength1kHz = lengthFunc(frequencyDomain, step)
      if (fskLength1kHz.length > 1) {
        frequencyDomain
          .dropWhile(_._1 % step != 0)
          .sliding(fskLengthFull, fskLength1kHz.length - 1)
          .toList
          .filter(_.length == fskLengthFull)
          .par
          .foreach { l =>
            val center = levelOne
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
              println(s"start freq: ${l.head._1} center lvl: ${center}_____b0: ${b0}__b4: ${b4}__b4: ${b4}__b12: ${b12}______b2: ${b2}__b6: ${b6}__b10: ${b10}")
            }
          }
      }
      loop()
    }

    if (!isCancelled.get()) {
      try {
        loop()
      } catch {
        case e: Exception => e.printStackTrace()
      }
    } else {
      println("completing detector task")
    }
  }
}
