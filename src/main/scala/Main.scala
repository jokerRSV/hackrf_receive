import scalafx.Includes._
import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Pos.TopCenter
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, CheckBox, ChoiceBox, Label}
import scalafx.scene.image.{ImageView, PixelFormat, PixelWriter, WritableImage}
import scalafx.scene.layout.{BorderPane, HBox, StackPane, VBox}
import scalafx.scene.paint.Color
import tasks.MainFskTask
import utils.JavaFXExecutionContext

import java.util.concurrent.TimeUnit

object Main extends JFXApp3 {
  val START_FREQUNCEY = 105300000 // in Hz
  var FFT_BIN_WIDTH = 100 // in Hz [20, 25, 40, 50, 100, 125, 150]
  val baseBinary = 1024
  var FFT_SIZE = 32 * baseBinary // in Hz
  val LNA = 16 // 0 to 40 with 8 step
  val VGA = 10 // 0 to 62 with 2 step
  val count = 1

  val APP_SIZE_X = 1650
  val APP_SIZE_Y = 750
  val widthImageView = APP_SIZE_X
  val heightImageView = APP_SIZE_Y
  val scaleYOffset = 600
  var task: Option[MainFskTask] = None
  //  = new MainFskTask(CENTER_FREQUNCEY, SAMPLE_RATE, FFT_BIT_WIDTH, LNA, VGA)
  //  var taskDetector = new DetectorFSKTask(pis)
  var freqOffset = 0
  var limitFreq = 0
  //  val bw = FFT_SIZE * FFT_BIN_WIDTH
  val bw = 0
  var isOn = true

  val coef = APP_SIZE_Y - 20
  val xOffset = 20
  val min = 0
  val max = 150
  val diff = max - min

  def createMainFskTask(pixelWriter: PixelWriter, startLabel: Label, endLabel: Label): Unit = {
    val endFreq = FFT_SIZE * FFT_BIN_WIDTH + START_FREQUNCEY
    val SAMPLE_RATE = FFT_BIN_WIDTH * FFT_SIZE // sample rate in Hz
    println(s"freq start: ${START_FREQUNCEY} bw: ${bw} end: ${endFreq}")
    println(s"fft bin width calc: ${FFT_BIN_WIDTH}")
    println(s"sample rate: ${SAMPLE_RATE}")
    val y = Array.fill(100)(1.toByte, 1.toByte, 1.toByte).flatMap(t => Array(t._1, t._2, t._3))
    val y1000R = Array.fill(100)(255.toByte, 0.toByte, 0.toByte).flatMap(t => Array(t._1, t._2, t._3))
    val y10000G = Array.fill(100)(50.toByte, 255.toByte, 50.toByte).flatMap(t => Array(t._1, t._2, t._3))
    val fillY = Array.fill(3)(0.toByte, 150.toByte, 255.toByte).flatMap(t => Array(t._1, t._2, t._3))
    val format = PixelFormat.getByteRgbInstance
    val roundedX = widthImageView / 100 * 100

    task match {
      case Some(value) =>
        value.cancel
        println(s"task is running: ${value.isRunning}")
        while (value.isRunning) {
          TimeUnit.MILLISECONDS.sleep(10)
        }
        TimeUnit.MILLISECONDS.sleep(1000)
      case None => println("no tasks")

    }
    //    this.START_FREQUNCEY = scaleX.head._3.toInt / 1000000
    task = Some(new MainFskTask(START_FREQUNCEY, SAMPLE_RATE, FFT_SIZE, LNA, VGA, bw, count, isOn))
    task.foreach { t =>
      t.valueProperty().addListener { (_, _, list) =>
        clearImage(widthImageView, heightImageView, pixelWriter)
        val cutList = list.drop(freqOffset / FFT_BIN_WIDTH)
        val scaleX =
          cutList
            .take(roundedX + 1)
            .map(t => (((t._1 - cutList.head._1) / FFT_BIN_WIDTH).toInt, t._2, t._1))
        //            .map(t => (((t._1 - cutList.head._1) / FFT_BIN_WIDTH).toInt, t._2, t._1))
        if (scaleX.nonEmpty) {
          startLabel.text = scaleX.head._3.toInt.toString
          this.limitFreq = scaleX.last._3.toInt
          endLabel.text = scaleX.last._3.toInt.toString
        }

        scaleX
          .foreach {
            case (currentX, currentY, currScale) =>
              //            if (currScale == scaleX.last._3 || currScale == scaleX.head._3) {
              /*draw start/end frequncy's scale*/
              //              pixelWriter.setPixels(currentX + xOffset, scaleYOffset - 2, 3, 20, format, y, 0, 0)
              //            }
              if (currentX < roundedX - 1) {
                //draw scale every 1M
                val base = currScale.toInt - START_FREQUNCEY
                if (base % 1000000 == 0) {
                  pixelWriter.setPixels(currentX + xOffset, scaleYOffset - 18, 4, 30, format, y, 0, 0)
                  //draw scale every 100k
                } else if (base % 100000 == 0) {
                  pixelWriter.setPixels(currentX + xOffset - 1, scaleYOffset - 5, 3, 15, format, y, 0, 0)
                  //draw scale every 10k
                } else if (base % 10000 == 0) {
                  pixelWriter.setPixels(currentX + xOffset - 1, scaleYOffset + 3, 3, 7, format, y10000G, 0, 0)
                  //draw scale every 1k
                } else if (base % 1000 == 0) {
                  pixelWriter.setPixels(currentX + xOffset - 1, scaleYOffset + 5, 3, 5, format, y1000R, 0, 0)
                }
                pixelWriter.setPixels(currentX + xOffset, scaleYOffset + 10, 1, 2, format, y, 0, 0)
              }

              val currYY = coef - (((currentY - min) / diff) * coef).toInt
              val currXX = currentX + xOffset
              if (currXX >= 0 && currYY >= 0 && currXX < roundedX + xOffset && currYY < heightImageView - 1) {
                pixelWriter.setColor(currXX, currYY, Color.Black)
                pixelWriter.setPixels(currXX, currYY, 1, scaleYOffset - 30 - currYY, format, fillY, 0, 0)
              }
            case _ => ()
          }
      }
      val thread = new Thread(t)
      thread.setName(s"${START_FREQUNCEY}_${FFT_BIN_WIDTH}_${FFT_SIZE}")
      thread.setDaemon(true)
      thread.start()
    }
  }

  private def clearImage(width: Int, height: Int, pixelWriter: PixelWriter) =
    for {
      x <- (0 until width)
      y <- (0 until height)
    } yield {
      pixelWriter.setColor(x, y, Color.White)
    }

  private def createImage(writableImage: WritableImage): ImageView = {
    val imageView = new ImageView()
    imageView.setImage(writableImage)
    imageView.margin = Insets(0, 0, 0, 0)
    imageView
  }

  override def start(): Unit = {
    val writableImage = new WritableImage(APP_SIZE_X, heightImageView)
    val pixelWriter = writableImage.getPixelWriter
    val imageView = createImage(writableImage)

    val hBoxForLabels = new HBox()
    hBoxForLabels.alignment = TopCenter
    //    hBoxForLabels.margin = Insets(10, 10, 10, 10)
    //    hBoxForLabels.setPadding(Insets(10, 10, 10, 10))
    val vBoxForControlPanel = new VBox()
    vBoxForControlPanel.alignment = TopCenter
    vBoxForControlPanel.margin = Insets(10, 10, 10, 10)
    vBoxForControlPanel.setPadding(Insets(10, 10, 10, 10))

    val startLabel = new Label("0")
    val endLabel = new Label("1000000")
    val lLowBtn = new Button("<<0.01")
    val rLowBtn = new Button("0.01>>")
    val lFastBtn = new Button("<<<1")
    val rFastBtn = new Button("1>>>")
    val lSlowBtn = new Button("<0.1")
    val rSlowBtn = new Button("0.1>")
    lLowBtn.setOnAction { _ =>
      val temp = this.freqOffset - 10000
      if (temp >= 0) {
        this.freqOffset = temp
      }
    }
    rLowBtn.setOnAction { _ =>
      val temp = this.freqOffset + 10000
      if (temp < this.limitFreq) {
        this.freqOffset = temp
      }
    }
    lFastBtn.setOnAction { _ =>
      val temp = this.freqOffset - 1000000
      if (temp >= 0) {
        this.freqOffset = temp
      }
    }
    rFastBtn.setOnAction { _ =>
      val temp = this.freqOffset + 1000000
      if (temp < this.limitFreq) {
        this.freqOffset = temp
      }
    }
    lSlowBtn.setOnAction { _ =>
      val temp = this.freqOffset - 100000
      if (temp >= 0) {
        this.freqOffset = temp
      }
    }
    rSlowBtn.setOnAction { _ =>
      val temp = this.freqOffset + 100000
      if (temp < this.limitFreq) {
        this.freqOffset = temp
      }
    }
    val fftSizeChoiceBox = new ChoiceBox[Int]()
    val list = ObservableBuffer.from(List(128, 64, 32, 16, 8, 4, 2, 1).map(_ * baseBinary))
    fftSizeChoiceBox.setItems(list)
    fftSizeChoiceBox.setValue(FFT_SIZE)
    fftSizeChoiceBox.getSelectionModel.selectedItemProperty().addListener { (_, _, newFftSize) =>
      this.FFT_SIZE = newFftSize
      createMainFskTask(pixelWriter, startLabel, endLabel)
    }
    val fftBinWidthChoiceBox = new ChoiceBox[Int]()
    val listBins = ObservableBuffer.from(List(20, 25, 40, 50, 100, 125, 200, 250, 500, 1000, 2000, 2500, 5000))
    fftBinWidthChoiceBox.setItems(listBins)
    fftBinWidthChoiceBox.setValue(FFT_BIN_WIDTH)
    fftBinWidthChoiceBox.getSelectionModel.selectedItemProperty().addListener { (_, _, newBinWidth) =>
      this.FFT_BIN_WIDTH = newBinWidth
      createMainFskTask(pixelWriter, startLabel, endLabel)
    }
    hBoxForLabels.children = List(lFastBtn, lSlowBtn, lLowBtn, rLowBtn, rSlowBtn, rFastBtn)
    val hBoxSecondPanel = new HBox()
    hBoxSecondPanel.alignment = TopCenter
    val onOffDisplay = new CheckBox()
    onOffDisplay.selected = this.isOn
    onOffDisplay.selectedProperty().addListener{ (_, _, nv) =>
      this.isOn = nv
      this.task.foreach(_.updateOnOff(this.isOn))
    }
    hBoxSecondPanel.children = List(fftSizeChoiceBox, fftBinWidthChoiceBox, onOffDisplay)
    vBoxForControlPanel.children = List(hBoxForLabels, hBoxSecondPanel)
    val stackPane = new StackPane()
    //    stackPane.prefWidth = widthImageView
    //    stackPane.prefHeight = heightImageView
    stackPane.children.addAll(imageView, startLabel, endLabel, vBoxForControlPanel)
    StackPane.setAlignment(vBoxForControlPanel, Pos.TopCenter)
    StackPane.setMargin(vBoxForControlPanel, Insets(0, 0, 0, 0))
    StackPane.setAlignment(startLabel, Pos.BottomLeft)
    StackPane.setAlignment(endLabel, Pos.BottomRight)
    StackPane.setMargin(startLabel, Insets(0, 5, 30, 25))
    StackPane.setMargin(endLabel, Insets(0, 25, 30, 5))

    val borderPane = new BorderPane()
    borderPane.padding = Insets(0, 0, 0, 0)
    borderPane.setCenter(stackPane)

    borderPane.setStyle("-fx-background-color: #e7e7e7")

    stage = new JFXApp3.PrimaryStage {
      title = s"hackrf"
      scene = new Scene() {
        stylesheets = List(getClass.getResource("caspian.css").toExternalForm)
        root = borderPane
      }
    }
    stage.setX(100)
    stage.setY(120)
    stage.resizable = false
    stage.onCloseRequest = _ => {
      JavaFXExecutionContext.customExecutor.shutdown()
      JavaFXExecutionContext.customExecutor.awaitTermination(10, TimeUnit.SECONDS)
    }

    stage.onCloseRequest = _ => {
      println("press cancel")
      task.foreach(_.cancel())
      println(s"the task state${task.map(_.isRunning)}")
    }

    createMainFskTask(pixelWriter, startLabel, endLabel)

  }
}
