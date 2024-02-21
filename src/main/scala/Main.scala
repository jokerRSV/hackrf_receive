import scalafx.Includes._
import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.event.ActionEvent
import scalafx.geometry.Pos.TopCenter
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.image.{ImageView, PixelFormat, PixelWriter, WritableImage}
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.layout.{BorderPane, HBox, StackPane, VBox}
import scalafx.scene.paint.Color
import tasks.{DetectorFSKTask, MainFskTask}
import utils.JavaFXExecutionContext

import java.util.concurrent.TimeUnit

object Main extends JFXApp3 {
  var START_FREQUNCEY = 105300000 // in Hz
  //  var SAMPLE_RATE = 2621440 // sample rate in Hz
  var SAMPLE_RATE = 20000000 // sample rate in Hz
  //  var FFT_BIN_WIDTH = 20 // in Hz [20, 25, 40, 50, 100, 125, 150]
  val baseBinary = 1024
  var FFT_SIZE = 128 / 2 * baseBinary // in Hz
  val LNA = 40 // 0 to 40 with 8 step
  val VGA = 16 // 0 to 62 with 2 step
  var counterLimit = 5
  var freqFactor = 0.03

  val APP_SIZE_X = 1650
  val APP_SIZE_Y = 750
  val widthImageView = APP_SIZE_X
  val heightImageView = APP_SIZE_Y
  val scaleYOffset = 600
  var task: Option[MainFskTask] = None
  var detectorFSKTask: Option[DetectorFSKTask] = None
  var freqOffset = 0
  var limitFreq = 0
  var isOn = true
  var ampEnable = false
  var levelOne = -90

  val coef = APP_SIZE_Y - 20
  val xOffset = 20
  val min = 0
  val max = 150
  val diff = max - min

  val yScaleB = Array.fill(xOffset * scaleYOffset)(1.toByte, 1.toByte, 1.toByte).flatMap(t => Array(t._1, t._2, t._3))
  val y1000000B = Array.fill(100)(1.toByte, 1.toByte, 1.toByte).flatMap(t => Array(t._1, t._2, t._3))
  val y100000B = Array.fill(100)(1.toByte, 1.toByte, 1.toByte).flatMap(t => Array(t._1, t._2, t._3))
  val y1000R = Array.fill(100)(255.toByte, 0.toByte, 0.toByte).flatMap(t => Array(t._1, t._2, t._3))
  val y10000G = Array.fill(100)(50.toByte, 255.toByte, 50.toByte).flatMap(t => Array(t._1, t._2, t._3))
  val fillY = Array.fill(3)(0.toByte, 150.toByte, 255.toByte).flatMap(t => Array(t._1, t._2, t._3))
  val format = PixelFormat.getByteRgbInstance
  val roundedX = widthImageView / 100 * 100

  def createMainFskTask(pixelWriter: PixelWriter, startLabel: Label, endLabel: Label, fftBinWidthLabel: Label): Unit = {
    //    val SAMPLE_RATE = FFT_BIN_WIDTH * FFT_SIZE // sample rate in Hz
    val endFreq = SAMPLE_RATE + START_FREQUNCEY
    val bw = 2500000
    //    val SAMPLE_RATE = 16384000 // sample rate in Hz
    println(s"freq start: ${START_FREQUNCEY} bw: ${endFreq - START_FREQUNCEY} end: ${endFreq}")
    println(s"sample rate: ${SAMPLE_RATE}")

    detectorFSKTask.foreach(_.cancel())
    task.foreach { value =>
      value.cancel
      println(s"main task is running: ${value.isRunning}")
      while (value.isRunning) {
        TimeUnit.MILLISECONDS.sleep(10)
      }
      TimeUnit.MILLISECONDS.sleep(100)
    }
    detectorFSKTask = Some(new DetectorFSKTask(START_FREQUNCEY, levelOne))
    task = Some(new MainFskTask(START_FREQUNCEY, SAMPLE_RATE, FFT_SIZE, LNA, VGA, bw, counterLimit, isOn, this.freqFactor, detectorFSKTask, this.ampEnable))
    //draw main y-axis
    pixelWriter.setPixels(xOffset - 10, 10, 2, scaleYOffset, format, yScaleB, 0, 0)

    task.foreach { t =>
      t.valueProperty().addListener { (_, _, list) =>
        val (fullList, fftBinWidth) = (list._1, list._2)
        fftBinWidthLabel.text = fftBinWidth.toString
        val cutList = fullList.drop(freqOffset / fftBinWidth.toInt)
        clearImage(pixelWriter)
        val scaleX =
          cutList
            .take(roundedX + 1)
            .map(t => (((t._1 - cutList.head._1) / fftBinWidth).toInt, t._2, t._1))
        if (scaleX.nonEmpty) {
          startLabel.text = scaleX.head._3.toInt.toString
          this.limitFreq = scaleX.last._3.toInt
          endLabel.text = s"${scaleX.last._3.toInt.toString}\n${endFreq}"
        }

        //draw main x-axis
        pixelWriter.setPixels(xOffset, scaleYOffset + 10, widthImageView, 2, format, yScaleB, 0, 0)
        scaleX
          .foreach {
            case (currentX, currentY, currScale) =>
              if (currentX < roundedX - 1) {
                //draw scale every 1M
                val base = currScale.toInt - START_FREQUNCEY
                if (base % 1000000 == 0) {
                  pixelWriter.setPixels(currentX + xOffset, scaleYOffset - 18, 4, 30, format, y1000000B, 0, 0)
                  //draw scale every 100k
                } else if (base % 100000 == 0) {
                  pixelWriter.setPixels(currentX + xOffset - 1, scaleYOffset - 5, 3, 15, format, y100000B, 0, 0)
                  //draw scale every 10k
                } else if (base % 10000 == 0) {
                  pixelWriter.setPixels(currentX + xOffset - 1, scaleYOffset + 3, 3, 7, format, y10000G, 0, 0)
                  //draw scale every 1k
                } else if (base % 1000 == 0) {
                  pixelWriter.setPixels(currentX + xOffset - 1, scaleYOffset + 5, 3, 5, format, y1000R, 0, 0)
                }
              }

              //val currYY = coef - (((currentY - min) / diff) * coef).toInt
              val currYY = (((-currentY - min) / diff) * coef).toInt
              val currXX = currentX + xOffset
              if (currXX >= 0 && currYY >= 0 && currXX < roundedX + xOffset && currYY < heightImageView - 1) {
//                pixelWriter.setColor(currXX, currYY, Color.Black)
                pixelWriter.setPixels(currXX, currYY, 1, scaleYOffset - 30 - currYY, format, fillY, 0, 0)
              }
            case _ => ()
          }
      }
      val thread = new Thread(t)
      thread.setName("MainFskTask")
      thread.setDaemon(true)
      thread.start()
    }
    detectorFSKTask.foreach { v =>
      val threadDetector = new Thread(v)
      threadDetector.setName("detector")
      threadDetector.setDaemon(true)
      threadDetector.start()
    }
  }

  val fillWhite = Array.fill(APP_SIZE_X * heightImageView)(255.toByte, 255.toByte, 255.toByte).flatMap(t => Array(t._1, t._2, t._3))

  private def clearImage(pixelWriter: PixelWriter) = {
    pixelWriter.setPixels(15, 20, widthImageView - 25, scaleYOffset, format, fillWhite, 0, 0)
  }

  private def createImage(writableImage: WritableImage): ImageView = {
    val imageView = new ImageView()
    imageView.setImage(writableImage)
    imageView.margin = Insets(0, 0, 0, 0)
    imageView
  }

  //  def createCommand(): Unit = {
  //    createMainFskTask(pixelWriter, startLabel, endLabel, fftBinWidthLabel)
  //  }

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

    val fftBinWidthLabel = new Label("fftBinWidth")
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
    val list = ObservableBuffer.from(List(1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1).map(_ * baseBinary))
    fftSizeChoiceBox.setItems(list)
    fftSizeChoiceBox.setValue(FFT_SIZE)
    fftSizeChoiceBox.getSelectionModel.selectedItemProperty().addListener { (_, _, newFftSize) =>
      this.FFT_SIZE = newFftSize
      createMainFskTask(pixelWriter, startLabel, endLabel, fftBinWidthLabel)
    }
    hBoxForLabels.children = List(lFastBtn, lSlowBtn, lLowBtn, rLowBtn, rSlowBtn, rFastBtn)
    val hBoxSecondPanel = new HBox()
    hBoxSecondPanel.alignment = TopCenter
    val counterLimitChoiceBox = new ChoiceBox[Int]()
    val counterLimitList = ObservableBuffer.from((1 to 100).toList)
    counterLimitChoiceBox.setItems(counterLimitList)
    counterLimitChoiceBox.setValue(counterLimit)
    counterLimitChoiceBox.getSelectionModel.selectedItemProperty().addListener { (_, _, newCounter) =>
      this.counterLimit = newCounter
      this.task.foreach(_.updateCounterLimit(this.counterLimit))
    }
    val onOffDisplay = new CheckBox()
    onOffDisplay.selected = this.isOn
    onOffDisplay.selectedProperty().addListener { (_, _, nv) =>
      this.isOn = nv
      this.task.foreach(_.updateOnOff(this.isOn))
    }

    val ampEnableCheckBox = new CheckBox()
    ampEnableCheckBox.selected = this.ampEnable
    ampEnableCheckBox.selectedProperty().addListener { (_, _, nv) =>
      this.ampEnable = nv
      createMainFskTask(pixelWriter, startLabel, endLabel, fftBinWidthLabel)
    }
    val slider = new Slider(0.000, 0.09, freqFactor)
    slider.margin = Insets(0, 0, 0, 2)
    slider.padding = Insets(10, 0, 0, 15)
    slider.setPrefSize(310, 45)
    slider.setShowTickLabels(true)
    slider.setShowTickMarks(true)
    slider.setMajorTickUnit(0.01)
    slider.setMinorTickCount(0)
    slider.setSnapToTicks(false)
    slider.setBlockIncrement(0.001)
    val fraction = Label(slider.getValue.toString)
    fraction.margin = Insets(10, 0, 0, 20)
    fraction.prefWidth = 130
    slider.value.addListener { (_, _, newV) =>
      val v = BigDecimal(newV.doubleValue()).setScale(3, BigDecimal.RoundingMode.HALF_UP)
      fraction.setText(v.toString())
      this.freqFactor = v.doubleValue
      this.task.foreach(_.updateFilterFactor(this.freqFactor))
    }

    val startFreqField = new TextField()
    //    startFreqField.setTextFormatter(new TextFormatter(new IntegerStringConverter()))
    startFreqField.prefWidth = 100
    startFreqField.prefHeight = 30
    startFreqField.setText(this.START_FREQUNCEY.toString)
    startFreqField.onKeyPressed = (action: KeyEvent) => {
      if (action.getCode == javafx.scene.input.KeyCode.ENTER) {
        this.START_FREQUNCEY = startFreqField.getText.toInt
        this.freqOffset = 0
        createMainFskTask(pixelWriter, startLabel, endLabel, fftBinWidthLabel)
      }
    }
    val sampleRateField = new TextField()
    //    startFreqField.setTextFormatter(new TextFormatter(new IntegerStringConverter()))
    sampleRateField.prefWidth = 80
    sampleRateField.prefHeight = 30
    sampleRateField.setText(this.SAMPLE_RATE.toString)
    sampleRateField.onKeyPressed = (action: KeyEvent) => {
      if (action.getCode == javafx.scene.input.KeyCode.ENTER) { // && action.getText.toInt <= 20000000 && action.getText.toInt >= 2000000) {
        if (sampleRateField.getText.toInt >= 2000000 && sampleRateField.getText.toInt <= 20000000) {
          this.SAMPLE_RATE = sampleRateField.getText.toInt
          this.freqOffset = 0
          createMainFskTask(pixelWriter, startLabel, endLabel, fftBinWidthLabel)
        }
      }
    }
    val sliderHBox = new HBox()
    sliderHBox.children = List(slider, fraction)
    hBoxSecondPanel.children = List(startFreqField, sampleRateField, fftSizeChoiceBox, counterLimitChoiceBox, onOffDisplay, fftBinWidthLabel)
    vBoxForControlPanel.children = List(hBoxForLabels, hBoxSecondPanel, sliderHBox)
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

    createMainFskTask(pixelWriter, startLabel, endLabel, fftBinWidthLabel)

  }
}
