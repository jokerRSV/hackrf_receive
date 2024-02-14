package utils

import javax.sound.sampled.{AudioFormat, AudioSystem, Mixer, TargetDataLine}
import scala.math.min
import scala.util.Try

object Utils {

  def calcCoefAmplBand(buff: Array[Double]): Double = {
    val bytes = buff.map(d => BigDecimal(d).setScale(2, BigDecimal.RoundingMode.DOWN).toDouble)
    val entropy = getEntropy(bytes)
    calcCoef(entropy)
    //        val b = buff
    //        val sum = b.sum
    //        sum / b.length
  }

  def calcCoefAmplBandL(buff: Seq[Double]): Double = {
    val bytes = buff.map(d => BigDecimal(d).setScale(2, BigDecimal.RoundingMode.DOWN).toDouble)
    val entropy = getEntropy(bytes)
    calcCoef(entropy)
    //        val b = buff
    //        val sum = b.sum
    //        sum / b.length
  }

  def findMean(buff: Seq[Double]): Double = {
    val bytes =
      buff
        .map(d => BigDecimal(d).setScale(2, BigDecimal.RoundingMode.DOWN).toDouble)
        .filter(_ > 0)
    val sum = bytes.sum
    if (sum > 0) {
      sum / bytes.length
    } else {
      0
    }
  }

  def findMeanF(buff: Seq[Float]): Double = {
    val bytes =
      buff
        .map(d => Math.abs(BigDecimal(d).setScale(2, BigDecimal.RoundingMode.DOWN).toDouble))
        .filter(_ > 0)
    val sum = bytes.sum
    if (sum > 0) {
      sum / bytes.length
    } else {
      0
    }
  }

  private def calcCoef(bytes: Map[Double, Int]): Double = {
    val l = bytes.toList.sortBy(_._1)
    val middle = l.map(_._1).max / 2d
    val (zeros, ones) = l.span(_._1 <= middle)
    val meanZeros = flattenBesides(zeros)
    val meanOnes = flattenBesides(ones)
    val mean = (meanZeros + meanOnes) / 2
    BigDecimal(mean).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  private def flattenBesides(bytes: List[(Double, Int)]): Double = {
    val sum = bytes.map(_._2).sum
    bytes
      .map(t => t._1 * t._2 / sum)
      .sum
  }

  def levenshtein(text: List[Char], word: List[Char]): Int = {
    val ii = (0 to word.length).toList
    text
      .foldLeft(ii) { (prev, x) =>
        prev.zip(prev.tail).zip(word).scanLeft(prev.head + 1) {
          case (h, ((d, v), y)) => min(min(h + 1, v + 1), d + (if (x == y) 0 else 1))
        }
      }
      .last
  }

  def getEntropy(bytes: Array[Double]): Map[Double, Int] = {
    bytes.foldLeft(Map.empty[Double, Int]) { (acc, el) =>
      val count = acc.getOrElse(el, 0)
      val newAcc = acc.updated(el, count + 1)
      newAcc
    }
  }

  def getEntropy(bytes: Seq[Double]): Map[Double, Int] = {
    bytes.foldLeft(Map.empty[Double, Int]) { (acc, el) =>
      val count = acc.getOrElse(el, 0)
      val newAcc = acc.updated(el, count + 1)
      newAcc
    }
  }

  def bytesToSamplesMixer(buff: Array[Byte], numBytes: Int): IndexedSeq[Double] = {
    //    val buffWithData = buff.take(numBytes)
    val dArr = for (i <- 0 until numBytes / 2) yield {
      ((buff(i * 2) & 0xff) << 8 | buff(i * 2 + 1) & 0xff).toShort / 32768d
    }
    dArr
  }

  def bytesToSamplesFile(buff: Array[Byte], numBytes: Int): IndexedSeq[Double] = {
    val dArr =
      for (i <- 0 until numBytes / 2) yield {
        val msb = buff(i * 2 + 1)
        val lsb = buff(i * 2)
        val full = ((msb & 0xff) << 8 | lsb & 0xff).toShort
        val res = full / 32768d
        res
      }
    dArr
  }

  def getLineFromMixer(mixer: Mixer): Option[TargetDataLine] = {
    val audioFormat = new AudioFormat(32000, 16, 1, true, true)
    mixer.getTargetLineInfo.flatMap { info =>
      Try {
        val line = mixer.getLine(info).asInstanceOf[TargetDataLine]
        line.open(audioFormat)
        line.stop()
        line.close()
        line.flush()
        line
      }.toOption
    }.headOption
  }

  def getDeviceByMixer(line: String): Option[Mixer] =
    AudioSystem.getMixerInfo
      .map(mixerInfo => AudioSystem.getMixer(mixerInfo))
      .find(_.getMixerInfo.getName == line)

  def getSupportedDeviceItems: Array[Mixer] = {
    val audioFormat = new AudioFormat(32000, 16, 1, true, true)
    val devices: Array[Mixer] =
      AudioSystem.getMixerInfo
        .map(mi => AudioSystem.getMixer(mi))
        .map { mixer =>
          val tdli = mixer.getTargetLineInfo
          (tdli.flatMap { li =>
            Try {
              val line = mixer.getLine(li).asInstanceOf[TargetDataLine]
              line.open(audioFormat)
              line.close()
            }.toOption
          }, mixer)
        }
        .filter(_._1.nonEmpty)
        .map(_._2)
    devices
  }

}
