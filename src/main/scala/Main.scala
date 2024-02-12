import javafx.collections.ObservableList
import mavlib.Batterworth2pLPF
import scalafx.Includes._
import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Pos.TopCenter
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ChoiceBox, Label}
import scalafx.scene.image.{ImageView, PixelFormat, PixelWriter, WritableImage}
import scalafx.scene.layout.{BorderPane, HBox, StackPane}
import scalafx.scene.paint.Color
import tasks.{DetectorFSKTask, MainFskTask}
import utils.JavaFXExecutionContext

import java.awt.Choice
import java.io.{PipedInputStream, PipedOutputStream}
import java.util.concurrent.TimeUnit

object Main extends JFXApp3 {
  val START_FREQUNCEY = 105 // in MHz
  val FFT_BIT_WIDTH = 5000 // in Hz
  val LNA = 40 // 0 to 40 with 8 step
  val VGA = 0 // 0 to 62 with 2 step

  val APP_SIZE_X = 1650
  val APP_SIZE_Y = 750
  val widthImageView = APP_SIZE_X
  val heightImageView = APP_SIZE_Y
  val offsetY = 0
  val scaleYOffset = 600
  var task = new MainFskTask(START_FREQUNCEY, FFT_BIT_WIDTH, LNA, VGA)
  //  var taskDetector = new DetectorFSKTask(pis)
  var freqOffset = 0
  var limitFreq = 0

  def createMainFskTask(pixelWriter: PixelWriter, startLabel: Label, endLabel: Label, fftBinWidth: Int): Unit = {
    val coef = APP_SIZE_Y - 20
    val xOffset = 20
    val min = 10
    val max = 250
    val diff = max - min
    val y = Array.fill(100)(1.toByte, 1.toByte, 1.toByte).flatMap(t => Array(t._1, t._2, t._3))
    val fillY = Array.fill(3)(0.toByte, 150.toByte, 255.toByte).flatMap(t => Array(t._1, t._2, t._3))
    val format = PixelFormat.getByteRgbInstance
    val roundedX = widthImageView / 100 * 100

    val filter = new Batterworth2pLPF()
    filter.setCutoffFreqFactor(0.05)

    task.cancel()
    while (task.isRunning) {
      TimeUnit.MILLISECONDS.sleep(10)
    }
    //    this.START_FREQUNCEY = scaleX.head._3.toInt / 1000000
    task = new MainFskTask(START_FREQUNCEY, fftBinWidth, LNA, VGA)

    task.valueProperty().addListener { (_, _, list) =>
      clearImage(widthImageView, heightImageView, pixelWriter)
      val cutList = list.drop(freqOffset / fftBinWidth)
      val scaleX =
        cutList
          .take(roundedX + 1)
          //          .map(t => (((t._1 - cutList.head._1) / FFT_BIT_WIDTH).toInt, filter.apply(t._2), t._1))
          .map(t => (((t._1 - cutList.head._1) / fftBinWidth).toInt, t._2, t._1))
      if (scaleX.nonEmpty) {
        startLabel.text = scaleX.head._3.toInt.toString
        this.limitFreq = scaleX.last._3.toInt
        endLabel.text = scaleX.last._3.toInt.toString
      }

      scaleX
        //        .sliding(2, 1)
        .toList
        .foreach {
          case (currentX, currentY, currScale) =>
            //            if (currScale == scaleX.last._3 || currScale == scaleX.head._3) {
            /*draw start/end frequncy's scale*/
            //              pixelWriter.setPixels(currentX + xOffset, scaleYOffset - 2, 3, 20, format, y, 0, 0)
            //            }
            if (currentX < roundedX - 1) {
              //draw scale every 1M
              if ((currScale.toInt - START_FREQUNCEY * 1000000) % 1000000 == 0) {
                pixelWriter.setPixels(currentX + xOffset, scaleYOffset - 18, 4, 30, format, y, 0, 0)
                //draw scale every 500k
              } else if ((currScale.toInt - START_FREQUNCEY * 1000000) % 500000 == 0) {
                pixelWriter.setPixels(currentX + xOffset - 1, scaleYOffset - 2, 2, 12, format, y, 0, 0)
                //draw scale every 100k
              } else if ((currScale.toInt - START_FREQUNCEY * 1000000) % 100000 == 0) {
                pixelWriter.setPixels(currentX + xOffset - 1, scaleYOffset + 5, 2, 5, format, y, 0, 0)
                //draw scale every 1k
              } else if ((currScale.toInt - START_FREQUNCEY * 1000000) % 1000 == 0) {
                pixelWriter.setPixels(currentX + xOffset - 1, scaleYOffset + 7, 2, 3, format, y, 0, 0)
              }
              pixelWriter.setPixels(currentX + xOffset, scaleYOffset + 10, 1, 2, format, y, 0, 0)
            }

            val currYY = (((Math.abs(currentY) - min) / diff) * coef).toInt + offsetY
            val currXX = currentX + xOffset
            if (currXX >= 0 && currYY >= 0 && currXX < roundedX + xOffset && currYY < heightImageView - 1) {
              pixelWriter.setColor(currXX, currYY, Color.Black)
              pixelWriter.setPixels(currXX, currYY, 1, scaleYOffset - 30 - currYY, format, fillY, 0, 0)
              //              val nextYY = (((Math.abs(nextY) - min) / diff) * coef).toInt + offsetY
              //              val nextXX = nextX + xOffset
              //              if (nextXX >= 0 && nextYY >= 0 && nextXX < roundedX + xOffset && nextYY < heightImageView - 1) {
              //                (Math.min(currYY, nextYY) to Math.max(currYY, nextYY)).foreach(yy => pixelWriter.setColor(currXX, yy, Color.Blue))
              //              }
            }
          case _ => ()
        }
    }
    val t = new Thread(task)
    t.setName(s"${START_FREQUNCEY}_${FFT_BIT_WIDTH}")
    t.setDaemon(true)
    t.start()
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
    hBoxForLabels.margin = Insets(10, 10, 10, 10)
    hBoxForLabels.setPadding(Insets(10, 10, 10, 10))

    val startLabel = new Label("0")
    val endLabel = new Label("1000000")
    val lMiddleBtn = new Button("<<0.5")
    val rMiddleBtn = new Button("0.5>>")
    val lFastBtn = new Button("<<<1")
    val rFastBtn = new Button("1>>>")
    val lSlowBtn = new Button("<0.1")
    val rSlowBtn = new Button("0.1>")
    val fftSize = new ChoiceBox[Int]()
    val list = ObservableBuffer.from(List(10000, 5000, 2000, 1000, 500, 200, 100, 50, 20))
    fftSize.setItems(list)
    fftSize.setValue(FFT_BIT_WIDTH)
    fftSize.getSelectionModel.selectedItemProperty().addListener { (_, ov, nv) =>
      createMainFskTask(pixelWriter, startLabel, endLabel, nv)
    }
    lMiddleBtn.setOnAction { _ =>
      val temp = this.freqOffset - 500000
      if (temp >= 0) {
        this.freqOffset = temp
      }
    }
    rMiddleBtn.setOnAction { _ =>
      val temp = this.freqOffset + 500000
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
    hBoxForLabels.children = List(lFastBtn, lMiddleBtn, lSlowBtn, rSlowBtn, rMiddleBtn, rFastBtn, fftSize)
    val stackPane = new StackPane()
    //    stackPane.prefWidth = widthImageView
    //    stackPane.prefHeight = heightImageView
    stackPane.children.addAll(imageView, startLabel, endLabel, hBoxForLabels)
    StackPane.setAlignment(hBoxForLabels, Pos.TopCenter)
    StackPane.setMargin(hBoxForLabels, Insets(0, 0, 0, 0))
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
      task.cancel()
    }

    createMainFskTask(pixelWriter, startLabel, endLabel, FFT_BIT_WIDTH)

  }
}
