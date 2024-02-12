package tasks

import javafx.concurrent.Task

import java.io.{ObjectInputStream, PipedInputStream}

class DetectorFSKTask(pis: PipedInputStream) extends Task[Unit] {

  val ois = new ObjectInputStream(pis)

  override def call(): Unit = {
    def loop(): Unit = {
//      println(ois.available())
//      ois.readObject()
    }

    if (!isCancelled) {
      loop()
    } else {
      println("completing detector task")
      ois.close()
    }
  }
}
