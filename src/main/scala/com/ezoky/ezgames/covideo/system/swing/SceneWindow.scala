/*
 * @author gweinbach on 30/06/2022 23:29
 * @since 0.2.0
 */

package com.ezoky.ezgames.covideo.system.swing


import com.ezoky.ez3d.Screen.*
import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Dimension.Ez3D.*
import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ezgames.covideo.component.HealthCondition.*
import com.ezoky.ezgames.covideo.component.swing.SwingSprite
import com.ezoky.ezgames.covideo.entity.People.{PersonId, Population}
import com.ezoky.ezgames.covideo.entity.{*, given}
import com.ezoky.ezgames.covideo.system.DisplaySystem

import java.awt.image.BufferedImage as AWTImage
import java.awt.{Graphics2D, GraphicsDevice, GraphicsEnvironment, Color as AWTColor, Dimension as AWTDimension, EventQueue as AWTEventQueue}
import javax.swing.border.Border
import javax.swing.{BorderFactory, JFrame, JPanel}
import scala.collection.mutable.Map as MutableMap

/**
 * @since 0.2.0
 * @author gweinbach on 30/06/2022
 */

private given Conversion[Pixel, Int] with
  def apply(pixel: Pixel): Int = pixel.asInt

extension (sceneDimension: ScreenDimension)
  private def awtDimension: AWTDimension =
    AWTDimension(sceneDimension.width, sceneDimension.height)

private class SceneWindow()
  extends JFrame:

  println("Creating a SceneWindow")

  private val panel: DrawingPanel = new DrawingPanel()
  private var frameSize: AWTDimension = new AWTDimension()

  initUI()

  def resize(size: ScreenDimension): Unit =
    if (size.awtDimension != frameSize)
      setSize(
        size.awtDimension
      )
      frameSize = size.awtDimension
      repaint()

  def updateTitle(newTitle: String): Unit =
    if (newTitle != getTitle)
      setTitle(newTitle)

  def draw(scene: Scene): Unit =
    panel.updateScene(scene)

  private def initUI(): Unit =
    getContentPane().add(panel)
    setSize(frameSize)
    setLocationRelativeTo(null) // centered on screen
    setResizable(false)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    setVisible(true)


private[swing] object SceneWindow:

  // singleton
  private val _SceneWindows: MutableMap[SceneId, SceneWindow] = MutableMap.empty

  def apply(sceneId: SceneId): SceneWindow =
    _SceneWindows.getOrElse(sceneId, {
      val mainWindow = new SceneWindow()
      _SceneWindows.addOne(sceneId, mainWindow)
      mainWindow
    })


/**
 * Drawing Panel
 */
private class DrawingPanel()
  extends JPanel:

  println("Creating a DrawingPanel")
  private var optScene: Option[Scene] = None

  import java.awt.{Graphics, Graphics2D, Toolkit}

  setDoubleBuffered(true)
  setBackground(AWTColor.black)

  override def paintComponent(g: Graphics): Unit =
    super.paintComponent(g)
    doDrawing(g)
    Toolkit.getDefaultToolkit.sync()

  private def doDrawing(g: Graphics): Unit =
    val g2d = g.asInstanceOf[Graphics2D]
    for
      scene <- optScene
    yield
      val pipeline3D = new Pipeline3D(scene.camera, scene)
      g2d.setColor(AWTColor.white)
      for
        component <- scene.components
      yield
        //        println(s"component=$component")
//        val componentTransformation =
//          new ComponentTransformation(component)
//            with ModelTransformation(component) {}

        val screenShape = pipeline3D.run(component)
        for
          vertex <- screenShape.vertices
        yield
          //          println(s"vertex=$vertex")
          g2d.drawLine(
            vertex.s.x,
            vertex.s.y,
            vertex.t.x,
            vertex.t.y,
          )

    //      for
    //        sprite <- scene.sprites.values
    //      yield
    //        val awtImage = sprite.image.asInstanceOf[AWTImage]

    //            for
    //              previousPosition <- sprite.previousPosition
    //            yield
    //              val previousScenePosition = scene.project(previousPosition)
    //              g2d.clearRect(
    //                previousScenePosition.x,
    //                previousScenePosition.y,
    ////                awtImage.getWidth(this),
    ////                awtImage.getHeight(this)
    //                20,20
    //              )

    //        val spritePosition = scene.project(sprite.position)
    //        g2d.drawImage(awtImage, spritePosition.x, spritePosition.y, this)

    g2d.dispose()


  def updateScene(scene: Scene): Unit =
    if (optScene.isEmpty)
      setPreferredSize(scene.preferredDimension.awtDimension)
      val border: Border = BorderFactory.createMatteBorder(
        scene.margins.top,
        scene.margins.left,
        scene.margins.bottom,
        scene.margins.right,
        AWTColor.black
      )
      setBorder(border)

    optScene = Some(scene)
    repaint()