package tasks

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.concurrent.duration.{Duration, NANOSECONDS}

class DetectorFSKTask(startFrequncyHz: Int, levelOne: Int) extends Runnable {
  println("init detector")
  val next: (Seq[(Double, Double)], Double, Int, Int) => Seq[(Double, Double)] =
    (xs, base, st, end) => xs.dropWhile(_._1 < base + st).takeWhile(_._1 <= base + end)

  val lengthFunc: (Array[(Double, Double)], Int) => Array[(Double, Double)] =
    (ar, i) => ar.takeWhile(_._1 <= startFrequncyHz + i)
  val step = 1000 // in Hz

  val atomicFreqDomain = new AtomicReference[(Array[(Double, Double)], Int)]((Array.fill(100)(0, 0), 0))
  val isCancelled = new AtomicBoolean(false)
  val coef = 6
  val taskList = (fftBinWidth: Int) => List(
    PairsBool(0, fftBinWidth * coef, condition = true),
    PairsBool(4000 - (fftBinWidth * coef), 4000 + (fftBinWidth * coef), condition = true),
    PairsBool(8000 - (fftBinWidth * coef), 8000 + (fftBinWidth * coef), condition = true),
    PairsBool(12000 - (fftBinWidth * coef), 12000, condition = true),
    PairsBool(1500, 2500, condition = false),
    PairsBool(5500, 6500, condition = false),
    PairsBool(9500, 10500, condition = false),
  )

  def updateFreqDomain(zippedFreqDomain: (Array[(Double, Double)], Int)): Unit = {
    val v = atomicFreqDomain.set(zippedFreqDomain)
    v
  }

  def cancel(): Unit = {
    println("stopping detector thread")
    isCancelled.set(true)
  }

  override def run(): Unit = {
    println(s"start detector thread ${Thread.currentThread().threadId()}")

    @tailrec
    def loop(): Unit = {
      //      println(s"working ${Thread.currentThread().threadId()}")
      //      val startTime = System.nanoTime()
      val (frequencyDomain, fftBinWidth) = atomicFreqDomain.get()
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
            lazy val center = levelOne
            lazy val headFreq = l.head._1
            lazy val slice = next.curried(l).apply(headFreq)
            lazy val b0 = slice(0)(fftBinWidth).min
            lazy val b4 = slice(4000 - (fftBinWidth * coef))(4000 + (fftBinWidth * coef)).min
            lazy val b8 = slice(8000 - (fftBinWidth * coef))(8000 + (fftBinWidth * coef)).min
            lazy val b12 = slice(12000 - (fftBinWidth * coef))(12000).min
            lazy val b2 = slice(1500)(2500).max
            lazy val b6 = slice(5500)(6500).max
            lazy val b10 = slice(9500)(10500).max
            //val res = b0 && b4 && b8 && b12 && b2 && b6 && b10
            val res = sliceFind(taskList(fftBinWidth), l)
            if (res) {
              println(s"start freq: ${l.head._1} center lvl: ${center}_____b0: ${b0}__b4: ${b4}__b8: ${b8}__b12: ${b12}______b2: ${b2}__b6: ${b6}__b10: ${b10}")
            }
          }
      }
      //      val endTime = System.nanoTime()
      //      println(s"diff time: ${Duration(endTime - startTime, NANOSECONDS).toMillis}")
      if (!isCancelled.get()) {
        loop()
      }
    }

    loop()
    println(s"completing detector thread ${Thread.currentThread().threadId()}")
  }

  def sliceFind(taskList: List[PairsBool], arr: Array[(Double, Double)]): Boolean = {
    val headFreq = arr.head._1

    @tailrec
    def loop(restTaskList: List[PairsBool], restArr: Array[(Double, Double)]): Boolean = {
      restTaskList match {
        case ::(head, next) =>
          val (first, rest) = restArr.dropWhile(_._1 < head.start + headFreq).span(_._1 <= head.end + headFreq)
          val res =
            if (head.condition) {
              first.exists(_._2 >= levelOne == head.condition)
            } else {
              first.forall(_._2 >= levelOne == head.condition)
            }
          if (res) {
            loop(next, rest)
          } else {
            false
          }
        case Nil => false
      }
    }

    loop(taskList, arr)
  }
}

case class PairsBool(start: Int, end: Int, condition: Boolean)