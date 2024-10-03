package com.ezoky.ezgames.covideo.system.javafx

import com.ezoky.ez3d.Screen.{Pixel, ScreenDimension}
import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable}
import com.ezoky.ezgames.covideo.entity.Scenes
import com.ezoky.ezgames.covideo.system.UserCommands
import javafx.geometry.Rectangle2D
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color

import scala.collection.mutable.Map as MutableMap


private given Conversion[Pixel, Double] with
  def apply(pixel: Pixel): Double = pixel.asInt.toDouble

extension (sceneDimension: ScreenDimension)
  private def javaFXRectangle2D: Rectangle2D =
    Rectangle2D(0.0, 0.0, sceneDimension.width, sceneDimension.height)

extension (javaFXRectangle2D: Rectangle2D)
  private def isNull: Boolean =
    (javaFXRectangle2D.getHeight == 0.0) &&
      (javaFXRectangle2D.getWidth == 0.0)

trait SceneWindows[I: Identifiable, D: Dimension]
  extends Scenes[I, D]
    with UserCommands[I, D]:

  import CoordsDimension.Ez3D.*

  private[javafx] class SceneWindow(userControlConfig: UserControlConfig)
    extends StackPane:

    private var _optScene: Option[Scene] = None
    private var _displayed: Boolean = false

    private val _layers = Array.fill(2)(new Canvas())
    private var _currentLayerIndex = 0

    private def _nextLayerIndex = if _currentLayerIndex == 0 then 1 else 0

    private def _currentLayer = _layers(_currentLayerIndex)

    private def _nextLayer = _layers(_nextLayerIndex)

    private def _switchLayers(): Unit = _currentLayerIndex = _nextLayerIndex

    def isDisplayed: Boolean =
      _displayed

//    def getHeight: Double =
//      _currentLayer.getHeight
//
//    def setHeight(height: Double): Unit =
//      _layers.map(_.setHeight(height))
//
//    def getWidth: Double =
//      _currentLayer.getWidth
//
//    def setWidth(width: Double): Unit =
//      _layers.map(_.setWidth(width))

    def resizeScene(size: ScreenDimension): Unit =
      val javaFXSize = size.javaFXRectangle2D
      if (javaFXSize.getHeight != getHeight) then
        _layers.map(_.setHeight(javaFXSize.getHeight))
        setHeight(javaFXSize.getHeight)
      if (javaFXSize.getWidth != getWidth) then
        _layers.map(_.setWidth(javaFXSize.getWidth))
        setWidth(javaFXSize.getWidth)

    def updateTitle(newTitle: String): Unit =
      MainWindow().setTitle(newTitle)

    private[javafx] def display(): Unit =
      getChildren().addAll(_currentLayer, _nextLayer)
      MainWindow().setScene(this)

      val gc = _nextLayer.getGraphicsContext2D()
      gc.setFill(Color.BLACK)
      gc.fillRect(0, 0, getWidth, getHeight)

      _displayed = true

    private[javafx] def draw(scene: Scene): Unit =
      _optScene = Some(scene)
      val gc = _nextLayer.getGraphicsContext2D()
      doDrawing(gc)
      _nextLayer.setVisible(true)
      _currentLayer.setVisible(false)
      _switchLayers()

    private def doDrawing(gc: GraphicsContext): Unit =

//      gc.clearRect(0, 0, getWidth, getHeight)
//      gc.setFill(Color.BLACK)
//      gc.fillRect(0, 0, getWidth, getHeight)

      for
        scene <- _optScene
      yield
        gc.setStroke(Color.WHITE)
        val pipeline3D = new Pipeline3D(scene.camera, scene)
        for
          component <- scene.components
        yield
          val screenShape = pipeline3D.run(component)
          for
            segment <- screenShape.segments
          yield
            //          println(s"segment=$segment")
            gc.strokeLine(
              segment.s.x,
              segment.s.y,
              segment.t.x,
              segment.t.y,
            )


  private[javafx] object SceneWindow:

    private val _SceneWindows: MutableMap[I, SceneWindow] = MutableMap.empty

    def apply(sceneId: I,
              userControlConfig: UserControlConfig): SceneWindow =
      _SceneWindows.getOrElse(sceneId, {
        val mainWindow = new SceneWindow(userControlConfig)
        _SceneWindows.addOne(sceneId, mainWindow)
        mainWindow
      })

    def all(): Iterable[SceneWindow] =
      _SceneWindows.values

