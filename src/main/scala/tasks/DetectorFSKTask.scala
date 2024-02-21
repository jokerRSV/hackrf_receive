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
  val levelOne = -85

  val atomicFreqDomain = new AtomicReference[Array[(Double, Double)]](Array.fill(100)(0, 0))
  val isCancelled = new AtomicBoolean(false)
  val taskList = List(
    PairsBool(0, 20, condition = true),
    PairsBool(3980, 4020, condition = true),
    PairsBool(7980, 8020, condition = true),
    PairsBool(11980, 12000, condition = true),
    PairsBool(1000, 3000, condition = false),
    PairsBool(5000, 7000, condition = false),
    PairsBool(9000, 11000, condition = false),
  )

  def updateFreqDomain(zippedFreqDomain: Array[(Double, Double)]): Unit = {
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
            lazy val center = levelOne
            lazy val headFreq = l.head._1
            lazy val slice = next.curried(l).apply(headFreq)
            lazy val b0 = slice(0)(20).min
            lazy val b4 = slice(3980)(4020).min
            lazy val b8 = slice(7980)(8020).min
            lazy val b12 = slice(11980)(12000).min
            lazy val b2 = slice(1000)(3000).max
            lazy val b6 = slice(5000)(7000).max
            lazy val b10 = slice(9000)(11000).max
            //val res = b0 && b4 && b8 && b12 && b2 && b6 && b10
            val res = sliceFind(taskList, l)
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
          val res = first.forall(_._2 >= levelOne == head.condition)
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