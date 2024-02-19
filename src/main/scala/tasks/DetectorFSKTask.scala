package tasks

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.concurrent.duration.{Duration, NANOSECONDS}

class DetectorFSKTask(startFrequncyHz: Int) extends Runnable {
  println("init detector")
  val next: (Seq[(Double, Double)], Double, Int, Int) => Seq[(Double, Double)] =
    (xs, base, st, end) => xs.dropWhile(_._1 < base + st).takeWhile(_._1 <= base + end)

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

    @tailrec
    def loop(): Unit = {
      //      val startTime = System.nanoTime()
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
            val slice = next.curried(l).apply(headFreq)
            lazy val b0 = slice(0)(20).forall(_._2 >= center)
            lazy val b4 = slice(3980)(4020).forall(_._2 >= center)
            lazy val b8 = slice(7980)(8020).forall(_._2 >= center)
            lazy val b12 = slice(11980)(12000).forall(_._2 >= center)
            lazy val b2 = slice(1000)(3000).forall(_._2 < center)
            lazy val b6 = slice(5000)(7000).forall(_._2 < center)
            lazy val b10 = slice(9000)(11000).forall(_._2 < center)
            val res = b0 && b4 && b8 && b12 && b2 && b6 && b10
            if (res) {
              println(s"start freq: ${l.head._1} center lvl: ${center}_____b0: ${b0}__b4: ${b4}__b8: ${b8}__b12: ${b12}______b2: ${b2}__b6: ${b6}__b10: ${b10}")
            }
          }
      }
      //      val endTime  = System.nanoTime()
      //      println(s"diff time: ${Duration(endTime - startTime, NANOSECONDS).toMillis}")
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
