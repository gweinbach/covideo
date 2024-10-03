package com.ezoky.ezgames.covideo.system.javafx

import javafx.application.{Application, Platform}
import javafx.scene.layout.{AnchorPane, HBox}
import javafx.scene.{Node, Scene}
import javafx.stage.Stage


private[javafx] class JavaFXMainWindw() extends Application:

  private val _sceneContainer: AnchorPane = new AnchorPane()
  private val _controlsContainer: AnchorPane = new AnchorPane()
  private var _stage: Stage = null

  override def start(stage: Stage): Unit =
    _stage = stage
    val mainLine = new HBox(_controlsContainer, _sceneContainer)
    val scene = new Scene(mainLine)
    stage.setScene(scene)
    stage.show()
    MainWindow.setStarted(this)

  private[javafx] def setScene(canvasNode: Node): Unit =
    MainWindow.fxRun {
      () =>
        if _sceneContainer.getChildren().size() == 0 ||
          _sceneContainer.getChildren().get(0) != canvasNode then
          _sceneContainer.getChildren().add(0, canvasNode)
    }

  private[javafx] def setControls(controlNode: Node): Unit =
    MainWindow.fxRun {
      () =>
        if _controlsContainer.getChildren().size() == 0 ||
          _controlsContainer.getChildren().get(0) != controlNode then
          _controlsContainer.getChildren().add(0, controlNode)
    }

  private[javafx] def setTitle(title: String): Unit =
    MainWindow.fxRun {
      () =>
        if _stage.getTitle != title then
          _stage.setTitle(title)
    }


object MainWindow:

  private var _mainWindow: JavaFXMainWindw = null

  def apply(): JavaFXMainWindw =
    MainWindow.synchronized {
      if _mainWindow == null then
        val mainJavaFX = new Runnable:
          override def run(): Unit =
            Application.launch(classOf[JavaFXMainWindw])
        Thread.currentThread()
        new Thread(mainJavaFX).start()
        // wait for _mainWindow
        try
          wait()
        catch
          case _: InterruptedException => {}
    }
    if _mainWindow == null then
      System.err.println("Java FX MainWindow is not set")
      Platform.exit()
    _mainWindow

  def setStarted(mainWindow: JavaFXMainWindw): Unit =
    MainWindow.synchronized {
      this._mainWindow = mainWindow
      MainWindow.notify()
    }

  def fxRun[T](code: () => T): Unit =
    Platform.runLater {
      new Runnable:
        override def run(): Unit = code()
    }
